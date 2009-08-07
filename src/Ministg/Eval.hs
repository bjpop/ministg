{-# OPTIONS_GHC -XPatternGuards #-}
module Ministg.Eval (run) where

import Control.Monad.State
import Control.Monad.Trans
import Data.Map as Map hiding (map)
import Data.List (foldl')
import Ministg.AST

data Continuation
   = CaseCont [Alt] 
   | UpdateCont Var
   | ArgCont Atom
   deriving (Eq, Show)

type Stack = [Continuation]
type Heap = Map.Map Var Object
data EvalState = EvalState { state_unique :: Int }
type Eval a = StateT EvalState IO a

initState :: EvalState
initState = EvalState { state_unique = 0 }

initHeap :: Program -> Heap
initHeap = Map.fromList

initStack :: Stack
initStack = []

lookupHeap :: Var -> Heap -> Object 
lookupHeap var heap = 
   case Map.lookup var heap of
      Nothing -> error $ "undefined variable: " ++ show var
      Just object -> object

lookupHeapAtom :: Atom -> Heap -> Object
lookupHeapAtom (Variable var) heap = lookupHeap var heap
lookupHeapAtom other _heap = error $ "lookupHeapAtom called with non variable " ++ show other

updateHeap :: Var -> Object -> Heap -> Heap 
updateHeap = Map.insert 

run :: Program -> IO ()
run decls = evalStateT (evalProgram $ initHeap decls) initState

freshVar :: Eval Var
freshVar = do
   u <- gets state_unique
   modify $ \s -> s { state_unique = u + 1 }
   return $ "$" ++ show u

evalProgram :: Heap -> Eval ()
evalProgram heap = do
   printFullResult (Variable "main") initStack heap
   liftIO $ putStr "\n"

printFullResult :: Atom -> Stack -> Heap -> Eval (Stack, Heap) 
printFullResult atom stack heap = do
   (Atom newAtom, newStack, newHeap) <- bigStep (Atom atom) stack heap
   case newAtom of
      (Literal (Integer i)) -> do
         liftIO $ putStr $ show i 
         return (newStack, newHeap)
      (Variable v) -> do
         case lookupHeap v newHeap of
            Fun {} -> do
               liftIO $ putStr "<function>"
               return (newStack, newHeap)
            Pap {} -> do
               liftIO $ putStr "<pap>"
               return (newStack, newHeap)
            Con constructor args -> do
               liftIO $ putStr $ "(" ++ constructor
               (finalStack, finalHeap) <- printArgs args newStack newHeap 
               liftIO $ putStr ")"
               return (finalStack, finalHeap)
        
printArgs :: [Atom] -> Stack -> Heap -> Eval (Stack, Heap)
printArgs [] stack heap = return (stack, heap)
printArgs (a:as) stack heap = do
   liftIO $ putStr " "
   (newStack, newHeap) <- printFullResult a stack heap
   printArgs as newStack newHeap

bigStep :: Exp -> Stack -> Heap -> Eval (Exp, Stack, Heap) 
bigStep exp stack heap = do
   -- liftIO $ print exp
   -- liftIO $ putStr "\n\n"
   -- liftIO $ print (exp, stack, heap) 
   result <- smallStep exp stack heap
   case result of
      Nothing -> return (exp, stack, heap)
      Just (newExp, newStack, newHeap) -> bigStep newExp newStack newHeap

smallStep :: Exp -> Stack -> Heap -> Eval (Maybe (Exp, Stack, Heap))
-- LET
smallStep (Let var object exp) stack heap = do
   newVar <- freshVar
   let newHeap = updateHeap newVar object heap
   let newExp = subs (mkSub var (Variable newVar)) exp 
   return $ Just (newExp, stack, newHeap)
-- CASECON
smallStep (Case (Atom (Variable v)) alts) stack heap
   | Con constructor args <- lookupHeap v heap, 
     Just (vars, exp) <- exactPatternMatch constructor alts =
        return $ Just (subs (mkSubList $ zip vars args) exp, stack, heap)
-- CASEANY
smallStep (Case (Atom v) alts) stack heap
   | isLiteral v || isValue (lookupHeapAtom v heap), 
     Just (x, exp) <- defaultPatternMatch alts =
        return $ Just (subs (mkSub x v) exp, stack, heap)
-- CASE
smallStep (Case exp alts) stack heap = return $ Just (exp, CaseCont alts : stack, heap)
-- RET 
smallStep exp@(Atom atom) (CaseCont alts : stackRest) heap
   | isLiteral atom || isValue (lookupHeapAtom atom heap) = 
        return $ Just (Case exp alts, stackRest, heap)
-- THUNK 
smallStep (Atom (Variable x)) stack heap
   | Thunk exp <- lookupHeap x heap = do
        let newHeap = updateHeap x BlackHole heap
        return $ Just (exp, UpdateCont x : stack, newHeap)
-- UPDATE
smallStep atom@(Atom (Variable y)) (UpdateCont x : stackRest) heap
   | object <- lookupHeap y heap, isValue object = 
        return $ Just (atom, stackRest, updateHeap x object heap)
-- KNOWNCALL
smallStep (FunApp (Just arity) var args) stack heap
   | arity == length args = 
        case lookupHeap var heap of
           Fun params body -> do
              let newBody = subs (mkSubList $ zip params args) body
              return $ Just (newBody, stack, heap) 
           other -> fail $ "known function " ++ var ++ " bound to non function object: " ++ show other
-- PRIMOP
smallStep (PrimApp prim args) stack heap = do
   -- (result, newStack, newHeap) <- evalPrimOuter prim args stack heap
   (result, newStack, newHeap) <- evalPrim prim args stack heap
   return $ Just (Atom result, newStack, newHeap)
-- PUSH
smallStep (FunApp _arity f args) stack heap = 
   return $ Just (Atom (Variable f), map ArgCont args ++ stack, heap)
-- FENTER
smallStep (Atom (Variable f)) stack heap
   | Fun vars exp <- lookupHeap f heap,
     argConts <- takeWhile isArgCont stack,
     length vars == length argConts = do
        let argAtoms = [atom | ArgCont atom <- argConts]
        return $ Just (subs (mkSubList $ zip vars argAtoms) exp, stack, heap) 
-- PAP1
smallStep (Atom (Variable f)) stack heap
   | Fun vars exp <- lookupHeap f heap,
     argConts <- takeWhile isArgCont stack,
     length argConts >= 1,
     length vars > length argConts = do
        let argAtoms = [atom | ArgCont atom <- argConts]
        p <- freshVar
        return $ Just (Atom (Variable p), stack, updateHeap p (Pap f argAtoms) heap)
-- PENTER
smallStep (Atom (Variable f)) stack@(ArgCont _ : stackRest) heap
   | Pap g args <- lookupHeap f heap =
        return $ Just (Atom (Variable g), map ArgCont args ++ stack, heap) 
smallStep _other _stack _heap = return Nothing

{-
evalPrimOuter :: Prim -> [Atom] -> Stack -> Heap -> Eval (Atom, Stack, Heap)
evalPrimOuter prim args stack heap = do
    (argVals, newStack, newHeap) <- forceArgs args stack heap
    evalPrim prim argVals newStack newHeap

forceArgs :: [Atom] -> Stack -> Heap -> Eval ([Atom], Stack, Heap)
forceArgs [] stack heap = return ([], stack, heap)
forceArgs (atom@(Literal i) : atoms) stack heap = do
   (results, newStack, newHeap) <- forceArgs atoms stack heap
   return (atom : results, newStack, newHeap)
forceArgs (atom@(Variable v) : atoms) stack heap = do
   (Atom value, newStack1, newHeap1) <- bigStep (Atom atom) stack heap
   (results, newStack2, newHeap2) <- forceArgs atoms newStack1 newHeap1 
   return (value : results, newStack2, newHeap2)
-}


evalPrim :: Prim -> [Atom] -> Stack -> Heap -> Eval (Atom, Stack, Heap)
evalPrim Add args stack heap = mkIntPrim (+) args stack heap
evalPrim Subtract args stack heap = mkIntPrim (-) args stack heap
evalPrim Multiply args stack heap = mkIntPrim (*) args stack heap
evalPrim Equality args stack heap = mkIntComparePrim (==) args stack heap
evalPrim LessThan args stack heap = mkIntComparePrim (<) args stack heap
evalPrim LessThanEquals args stack heap = mkIntComparePrim (<=) args stack heap
evalPrim GreaterThan args stack heap = mkIntComparePrim (>) args stack heap
evalPrim GreaterThanEquals args stack heap = mkIntComparePrim (>=) args stack heap
evalPrim PrintInt [arg@(Literal (Integer i))] stack heap = do
   liftIO $ print i
   return (arg, stack, heap)
evalPrim IntToBool [Literal (Integer i)] stack heap = do
   var <- freshVar
   let newHeap = updateHeap var (Con (if i == 1 then "True" else "False") []) heap
   return (Variable var, stack, newHeap)
evalPrim prim args stack heap = error $ show (prim,args)

exactPatternMatch :: Constructor -> [Alt] -> Maybe ([Var], Exp)
exactPatternMatch con1 (PatAlt con2 vars exp : alts)
   | con1 == con2 = Just (vars, exp)
   | otherwise = exactPatternMatch con1 alts
exactPatternMatch con (DefaultAlt {} : _) = Nothing
exactPatternMatch _con [] = Nothing

defaultPatternMatch :: [Alt] -> Maybe (Var, Exp)
defaultPatternMatch [] = Nothing
defaultPatternMatch (PatAlt {} : alts) = defaultPatternMatch alts
defaultPatternMatch (DefaultAlt var exp : _alts) = Just (var, exp) 

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

mkIntPrim :: (Integer -> Integer -> Integer) -> [Atom] -> Stack -> Heap -> Eval (Atom, Stack, Heap)
mkIntPrim op [Literal (Integer i), Literal (Integer j)] stack heap
   = return (Literal $ Integer (i `op` j), stack, heap)
{-
   = do v <- freshVar
        let newHeap = updateHeap v (Con "I" [Literal $ Integer (i `op` j)])
        return (Variable v, stack, heap)  
-}

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
