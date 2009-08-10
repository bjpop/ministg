{-# OPTIONS_GHC -XPatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.Eval
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Evaluate a ministg program using the semantics described in the 
-- "fast curry" paper by Simon Marlow and Simon Peyton Jones.
-----------------------------------------------------------------------------

module Ministg.Eval (run, Style(..)) where

import Control.Monad.State
import Control.Monad.Trans
import Data.Map as Map hiding (map)
import Data.List (foldl')
import Ministg.AST

-- | Stack continuations.
data Continuation
   = CaseCont [Alt]  -- ^ The alternatives of a case expression.
   | UpdateCont Var  -- ^ A variable which points to a thunk to be updated.
   | ArgCont Atom    -- ^ A pending argument (used only by the push-enter model).
   deriving (Eq, Show)

-- | The evaluation stack. 
type Stack = [Continuation]
-- | The heap (mapping variables to objects).
type Heap = Map.Map Var Object
-- | State to be threaded through evaluation.
data EvalState = EvalState { state_unique :: Int }
-- | Eval monad. Combines State and IO.
type Eval a = StateT EvalState IO a
-- | The style of semantics: push-enter or eval-apply
data Style
   = PushEnter
   | EvalApply
   deriving (Eq, Show)

initState :: EvalState
initState = EvalState { state_unique = 0 }

initHeap :: Program -> Heap
initHeap = Map.fromList

initStack :: Stack
initStack = []

-- | Lookup a variable in a heap. If found return the corresponding
-- object, otherwise throw an error (it is a fatal error which can't
-- be recovered from).
lookupHeap :: Var -> Heap -> Object 
lookupHeap var heap = 
   case Map.lookup var heap of
      Nothing -> error $ "undefined variable: " ++ show var
      Just object -> object

-- | Convenience wrapper for lookupHeap, for atoms which happen to be variables.
lookupHeapAtom :: Atom -> Heap -> Object
lookupHeapAtom (Variable var) heap = lookupHeap var heap
lookupHeapAtom other _heap = error $ "lookupHeapAtom called with non variable " ++ show other

-- | Add a new mapping to a heap, or update an existing one.
updateHeap :: Var -> Object -> Heap -> Heap 
updateHeap = Map.insert 

-- | Evaluate a ministg program and cause its effects to happen.
run :: Style -> Program -> IO ()
run style decls = evalStateT (evalProgram style $ initHeap decls) initState

-- | Generate a new unique variable. Uniqueness is guaranteed by using a
-- "$" prefix, which is not allowed in the concrete sytax of ministg programs.
freshVar :: Eval Var
freshVar = do
   u <- gets state_unique
   modify $ \s -> s { state_unique = u + 1 }
   return $ "$" ++ show u

evalProgram :: Style -> Heap -> Eval ()
evalProgram style heap = do
   printFullResult style (Variable "main") initStack heap
   liftIO $ putStr "\n"

-- | Top-level driver for evaluating a program. This ensures that the whole
-- top-level result is evaluated to normal form (not just WHNF). The value
-- of the result is pretty-printed as output. This is akin to the print 
-- part of a Read-Eval-Print evaluator. Note: this function is not tail-recursive
-- so it could use a lot of stack space for large answers.
printFullResult :: Style -> Atom -> Stack -> Heap -> Eval (Stack, Heap) 
printFullResult style atom stack heap = do
   (Atom newAtom, newStack, newHeap) <- bigStep style (Atom atom) stack heap
   case newAtom of
      (Literal (Integer i)) -> do
         liftIO $ putStr $ show i 
         return (newStack, newHeap)
      (Variable v) -> do
         case lookupHeap v newHeap of
            -- We don't look inside functions or paps. XXX For debugging purposes it
            -- might be nicer to print more than "<function>".
            Fun {} -> do
               liftIO $ putStr "<function>"
               return (newStack, newHeap)
            Pap {} -> do
               liftIO $ putStr "<pap>"
               return (newStack, newHeap)
            Con constructor args -> do
               liftIO $ putStr $ "(" ++ constructor
               -- print the arguments of the constructor.
               -- XXX Note the lack of tail recursion.
               (finalStack, finalHeap) <- printArgs style args newStack newHeap 
               liftIO $ putStr ")"
               return (finalStack, finalHeap)
        
-- | Recursively print (and hence evaluate) the arguments of a data constructor.
printArgs :: Style -> [Atom] -> Stack -> Heap -> Eval (Stack, Heap)
printArgs _style [] stack heap = return (stack, heap)
printArgs style (a:as) stack heap = do
   liftIO $ putStr " "
   (newStack, newHeap) <- printFullResult style a stack heap
   printArgs style as newStack newHeap

-- | Reduce an exression to WHNF (a big step reduction, which may be composed
-- of one or more small step reductions).
bigStep :: Style -> Exp -> Stack -> Heap -> Eval (Exp, Stack, Heap) 
bigStep style exp stack heap = do
   result <- smallStep style exp stack heap
   case result of
      -- Nothing more to do, we have reached a WHNF value (or perhaps some error).
      Nothing -> return (exp, stack, heap)
      -- There might be more to do, keep trying.
      Just (newExp, newStack, newHeap) -> bigStep style newExp newStack newHeap

-- | Perform one step of reduction. These equations correspond to the
-- rules in the operational semantics described in the "fast curry" paper.
smallStep :: Style -> Exp -> Stack -> Heap -> Eval (Maybe (Exp, Stack, Heap))
-- LET
smallStep _anyStyle (Let var object exp) stack heap = do
   newVar <- freshVar
   let newHeap = updateHeap newVar object heap
   let newExp = subs (mkSub var (Variable newVar)) exp 
   return $ Just (newExp, stack, newHeap)
-- CASECON
smallStep _anyStyle (Case (Atom (Variable v)) alts) stack heap
   | Con constructor args <- lookupHeap v heap, 
     Just (vars, exp) <- exactPatternMatch constructor alts =
        return $ Just (subs (mkSubList $ zip vars args) exp, stack, heap)
-- CASEANY
smallStep _anyStyle (Case (Atom v) alts) stack heap
   | isLiteral v || isValue (lookupHeapAtom v heap), 
     Just (x, exp) <- defaultPatternMatch alts =
        return $ Just (subs (mkSub x v) exp, stack, heap)
-- CASE
smallStep _anyStyle (Case exp alts) stack heap = return $ Just (exp, CaseCont alts : stack, heap)
-- RET 
smallStep _anyStyle exp@(Atom atom) (CaseCont alts : stackRest) heap
   | isLiteral atom || isValue (lookupHeapAtom atom heap) = 
        return $ Just (Case exp alts, stackRest, heap)
-- THUNK 
smallStep _anyStyle (Atom (Variable x)) stack heap
   | Thunk exp <- lookupHeap x heap = do
        let newHeap = updateHeap x BlackHole heap
        return $ Just (exp, UpdateCont x : stack, newHeap)
-- UPDATE
smallStep _anyStyle atom@(Atom (Variable y)) (UpdateCont x : stackRest) heap
   | object <- lookupHeap y heap, isValue object = 
        return $ Just (atom, stackRest, updateHeap x object heap)
-- KNOWNCALL
smallStep _anyStyle (FunApp (Just arity) var args) stack heap
   | arity == length args = 
        case lookupHeap var heap of
           Fun params body -> do
              let newBody = subs (mkSubList $ zip params args) body
              return $ Just (newBody, stack, heap) 
           other -> fail $ "known function " ++ var ++ " bound to non function object: " ++ show other
-- PRIMOP
smallStep _anyStyle (PrimApp prim args) stack heap = do
   (result, newStack, newHeap) <- evalPrim prim args stack heap
   return $ Just (Atom result, newStack, newHeap)
-- PUSH
smallStep PushEnter (FunApp _arity f args) stack heap = 
   return $ Just (Atom (Variable f), map ArgCont args ++ stack, heap)
-- FENTER
smallStep PushEnter (Atom (Variable f)) stack heap
   | Fun vars exp <- lookupHeap f heap,
     argConts <- takeWhile isArgCont stack,
     length vars == length argConts = do
        let argAtoms = [atom | ArgCont atom <- argConts]
        return $ Just (subs (mkSubList $ zip vars argAtoms) exp, stack, heap) 
-- PAP1
smallStep PushEnter (Atom (Variable f)) stack heap
   | Fun vars exp <- lookupHeap f heap,
     argConts <- takeWhile isArgCont stack,
     length argConts >= 1,
     length vars > length argConts = do
        let argAtoms = [atom | ArgCont atom <- argConts]
        p <- freshVar
        return $ Just (Atom (Variable p), stack, updateHeap p (Pap f argAtoms) heap)
-- PENTER
smallStep PushEnter (Atom (Variable f)) stack@(ArgCont _ : stackRest) heap
   | Pap g args <- lookupHeap f heap =
        return $ Just (Atom (Variable g), map ArgCont args ++ stack, heap) 
smallStep _anyStyle _other _stack _heap = return Nothing

-- | Evaluate the application of a primitive function. It is assumed that the
-- arguments of the primitive are already evaluated. Note: we allow primitives
-- to manipulate the heap and stack, but the semantics in the "fast curry" paper
-- do not.
evalPrim :: Prim -> [Atom] -> Stack -> Heap -> Eval (Atom, Stack, Heap)
evalPrim Add args stack heap = mkIntPrim (+) args stack heap
evalPrim Subtract args stack heap = mkIntPrim (-) args stack heap
evalPrim Multiply args stack heap = mkIntPrim (*) args stack heap
evalPrim Equality args stack heap = mkIntComparePrim (==) args stack heap
evalPrim LessThan args stack heap = mkIntComparePrim (<) args stack heap
evalPrim LessThanEquals args stack heap = mkIntComparePrim (<=) args stack heap
evalPrim GreaterThan args stack heap = mkIntComparePrim (>) args stack heap
evalPrim GreaterThanEquals args stack heap = mkIntComparePrim (>=) args stack heap
evalPrim IntToBool [Literal (Integer i)] stack heap = do
   var <- freshVar
   let newHeap = updateHeap var (Con (if i == 1 then "True" else "False") []) heap
   return (Variable var, stack, newHeap)
evalPrim prim args stack heap = error $ show (prim,args)

-- | Check for an exact pattern match for a data constructor in a list of case alternatives.
exactPatternMatch :: Constructor -> [Alt] -> Maybe ([Var], Exp)
exactPatternMatch con1 (PatAlt con2 vars exp : alts)
   | con1 == con2 = Just (vars, exp)
   | otherwise = exactPatternMatch con1 alts
exactPatternMatch con (DefaultAlt {} : _) = Nothing
exactPatternMatch _con [] = Nothing

-- | Check for a default pattern match (x -> e) in a list of case alternatives.
defaultPatternMatch :: [Alt] -> Maybe (Var, Exp)
defaultPatternMatch [] = Nothing
defaultPatternMatch (PatAlt {} : alts) = defaultPatternMatch alts
defaultPatternMatch (DefaultAlt var exp : _alts) = Just (var, exp) 

-- | Test for objects which denote values (WHNF values).
isValue :: Object -> Bool
isValue (Fun {}) = True
isValue (Pap {}) = True
isValue (Con {}) = True
isValue _other = False

isLiteral :: Atom -> Bool
isLiteral (Literal {}) = True
isLiteral _other = False

isArgCont :: Continuation -> Bool
isArgCont (ArgCont {}) = True
isArgCont _other = False

-- | Convenience function for making integer primitives. 
mkIntPrim :: (Integer -> Integer -> Integer) -> [Atom] -> Stack -> Heap -> Eval (Atom, Stack, Heap)
mkIntPrim op [Literal (Integer i), Literal (Integer j)] stack heap
   = return (Literal $ Integer (i `op` j), stack, heap)

-- | Convenience function for making integer comparison primitives. 
mkIntComparePrim :: (Integer -> Integer -> Bool) -> [Atom] -> Stack -> Heap -> Eval (Atom, Stack, Heap)
mkIntComparePrim op args stack heap = mkIntPrim (\i j -> if i `op` j then 1 else 0) args stack heap

type Substitution = Map.Map Var Atom

mkSub :: Var -> Atom -> Substitution 
mkSub = Map.singleton

mkSubList :: [(Var, Atom)] -> Substitution
mkSubList = Map.fromList

removeVars :: [Var] -> Substitution -> Substitution 
removeVars vars sub = foldl' (flip Map.delete) sub vars

class Substitute a where
   subs :: Substitution -> a -> a 

instance Substitute a => Substitute [a] where
   subs s = map (subs s)

subsVar :: Substitution -> Var -> Var 
subsVar s var = 
   case Map.lookup var s of
      Nothing -> var
      Just (Variable newVar) -> newVar
      Just (Literal lit) -> error $ "attempt to substitute variable " ++ var ++ " with literal " ++ show lit

instance Substitute Atom where
   subs s v@(Variable var) = 
      case Map.lookup var s of
         Nothing -> v
         Just atom -> atom 
   subs _s l@(Literal _) = l

instance Substitute Exp where
   subs s (Atom a) = Atom $ subs s a
   subs s exp@(FunApp arity var atoms)
      = FunApp arity (subsVar s var) (subs s atoms)
   subs s (PrimApp prim args) = PrimApp prim $ subs s args
   -- lets are not recursive so we don't really need to removeVars from s
   -- in the subs of obj, but it is safe to do so, and we might use it
   -- if lets become recursive.
   subs s exp@(Let var obj body)
      = Let var (subs newSub obj) (subs newSub body)
      where
      newSub = removeVars [var] s
   subs s (Case exp alts) = Case (subs s exp) (subs s alts)

instance Substitute Alt where
   subs s p@(PatAlt cons vars exp)
      = PatAlt cons vars $ subs (removeVars vars s) exp
   subs s p@(DefaultAlt var exp)
      = DefaultAlt var $ subs (removeVars [var] s) exp

instance Substitute Object where
   subs s f@(Fun args exp)
      = Fun args $ subs (removeVars args s) exp
   subs s (Pap var atoms)
      = Pap (subsVar s var) (subs s atoms)
   subs s (Con constructor atoms) = Con constructor $ subs s atoms
   subs s (Thunk exp) = Thunk $ subs s exp
   subs s BlackHole = BlackHole
