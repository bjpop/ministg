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

module Ministg.Eval (run) where

import Control.Monad.State (evalStateT, gets)
import Control.Monad.Trans (liftIO)
import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')
import Ministg.AST
import Ministg.CallStack (CallStack, push, showCallStack)
import Ministg.Pretty
import Ministg.State
import Ministg.TraceEval (traceEval, traceEnd)
import Ministg.Options as Opts 
       (Flag (..), EvalStyle (..), defaultEvalStyle, probeFlagsFirst, getEvalStyle)
import Ministg.GC (garbageCollect)

-- | Evaluate a ministg program and cause its effects to happen.
run :: [Flag] -> Program -> IO ()
run flags decls = 
   evalStateT (evalProgram style $ initHeap decls) (initState flags)
   where
   style = getEvalStyle flags

evalProgram :: EvalStyle -> Heap -> Eval ()
evalProgram style heap = do
   (newExp, _newStack, newHeap) <- bigStep style (Atom (Variable "main")) initStack heap
   traceEnd
   str <- case newExp of
             Atom (Literal lit) -> return $ prettyText lit 
             Atom (Variable var) -> do
                let object = lookupHeap var newHeap
                case object of
                   Error -> do
                      cs <- gets state_callStack
                      return $ "Exception raised!" ++ displayCallStack cs
                   other -> return $ prettyHeapObject newHeap $ lookupHeap var newHeap
             other -> return $ "Runtime error: result of bigStep is not an atom: " ++ show other 
   liftIO $ putStrLn str
   where
   displayCallStack [] = []
   displayCallStack cs = "\n\nCall stack:\n" ++ showCallStack cs
        
-- | Reduce an exression to WHNF (a big step reduction, which may be composed
-- of one or more small step reductions).
bigStep :: EvalStyle -> Exp -> Stack -> Heap -> Eval (Exp, Stack, Heap) 
bigStep style exp stack heap = do
   gcHeap <- garbageCollect exp stack heap
   traceEval exp stack gcHeap 
   result <- smallStep style exp stack gcHeap
   incStepCount
   case result of
      -- Nothing more to do, we have reached a WHNF value (or perhaps some error).
      Nothing -> return (exp, stack, gcHeap)
      -- There might be more to do, keep trying.
      Just (newExp, newStack, newHeap) -> bigStep style newExp newStack newHeap

-- | Perform one step of reduction. These equations correspond to the
-- rules in the operational semantics described in the "fast curry" paper.
smallStep :: EvalStyle -> Exp -> Stack -> Heap -> Eval (Maybe (Exp, Stack, Heap))
-- STACK ANNOTATION
smallStep style (Stack annotation exp) stack heap = do
   setRule "STACK"
   pushCallStack annotation
   return $ Just (exp, stack, heap)
-- LET
smallStep _anyStyle (Let var object exp) stack heap = do
   setRule "LET"
   newVar <- freshVar
   callStack <- gets state_callStack
   let annotatedObject = setThunkStack callStack object
   let newHeap = updateHeap newVar annotatedObject heap
   let newExp = subs (mkSub var (Variable newVar)) exp 
   return $ Just (newExp, stack, newHeap)
-- CASECON
smallStep _anyStyle (Case (Atom (Variable v)) alts) stack heap
   | Con constructor args <- lookupHeap v heap, 
     Just (vars, exp) <- exactPatternMatch constructor alts = do
        setRule "CASECON"
        return $ Just (subs (mkSubList $ zip vars args) exp, stack, heap)
-- CASEANY
smallStep _anyStyle (Case (Atom v) alts) stack heap
   | isLiteral v || isValue (lookupHeapAtom v heap) =
        case defaultPatternMatch alts of
           Just (x, exp) -> do
              setRule "CASEANY"
              return $ Just (subs (mkSub x v) exp, stack, heap)
           -- technically the compiler should insert a catch-all default alternative
           -- for each case expression, but if we don't check for it here we
           -- could have non-desirable error behaviour such as an infinite loop.
           Nothing -> fail "non exhaustive patterns in case expression"
-- CASE
smallStep _anyStyle (Case exp alts) stack heap = do
   setRule "CASE"
   callStack <- gets state_callStack
   return $ Just (exp, CaseCont alts callStack : stack, heap)
-- RET 
smallStep _anyStyle exp@(Atom atom) (CaseCont alts oldCallStack : stackRest) heap
   | isLiteral atom || isValue (lookupHeapAtom atom heap) = do
        setRule "RET"
        setCallStack oldCallStack
        return $ Just (Case exp alts, stackRest, heap)
-- THUNK 
smallStep _anyStyle (Atom (Variable x)) stack heap
   | Thunk exp thunkCallStack <- lookupHeap x heap = do
        setRule "THUNK"
        let newHeap = updateHeap x BlackHole heap
        oldCallStack <- gets state_callStack
        setCallStack thunkCallStack 
        return $ Just (exp, UpdateCont x oldCallStack : stack, newHeap)
-- UPDATE
smallStep _anyStyle atom@(Atom (Variable y)) (UpdateCont x oldCallStack : stackRest) heap
   | object <- lookupHeap y heap, isValue object = do
        setRule "UPDATE"
        setCallStack oldCallStack
        return $ Just (atom, stackRest, updateHeap x object heap)
-- KNOWNCALL
smallStep _anyStyle (FunApp (Just arity) var args) stack heap
   | arity == length args = 
        case lookupHeap var heap of
           Fun params body -> do
              setRule "KNOWNCALL"
              let newBody = subs (mkSubList $ zip params args) body
              return $ Just (newBody, stack, heap) 
           other -> fail $ "known function " ++ var ++ " bound to non function object: " ++ show other
-- PRIMOP
smallStep _anyStyle (PrimApp prim args) stack heap = do
   setRule "PRIMOP"
   (result, newStack, newHeap) <- evalPrim prim args stack heap
   return $ Just (Atom result, newStack, newHeap)

-- The push enter specific rules.

-- PUSH
smallStep PushEnter (FunApp _arity f args) stack heap = do
   setRule "PUSH"
   return $ Just (Atom (Variable f), map ArgCont args ++ stack, heap)
-- FENTER
smallStep PushEnter (Atom (Variable f)) stack heap
   | Fun vars exp <- lookupHeap f heap,
     (argConts, restStack) <- span isArgCont stack,
     length vars <= length argConts = do
        setRule "FENTER"
        let (enoughArgs, restArgs) = splitAt (length vars) argConts
        let argAtoms = [atom | ArgCont atom <- enoughArgs]
        let newStack = restArgs ++ restStack 
        return $ Just (subs (mkSubList $ zip vars argAtoms) exp, newStack, heap) 
-- PAP1
smallStep PushEnter (Atom (Variable f)) stack heap
   | Fun vars exp <- lookupHeap f heap,
     argConts <- takeWhile isArgCont stack,
     length argConts >= 1,
     length vars > length argConts = do
        setRule "PAP1"
        let argAtoms = [atom | ArgCont atom <- argConts]
        p <- freshVar
        return $ Just (Atom (Variable p), drop (length argConts) stack, updateHeap p (Pap f argAtoms) heap)
-- PENTER
smallStep PushEnter (Atom (Variable f)) stack@(ArgCont _ : stackRest) heap
   | Pap g args <- lookupHeap f heap = do
        setRule "PENTER"
        return $ Just (Atom (Variable g), map ArgCont args ++ stack, heap) 

-- The eval apply rules

-- EXACT
smallStep EvalApply (FunApp Nothing f args) stack heap
   | Fun vars exp <- lookupHeap f heap, length args == length vars = do
        setRule "EXACT"
        let newExp = subs (mkSubList $ zip vars args) exp
        return $ Just (newExp, stack, heap)
-- CALLK
smallStep EvalApply (FunApp _anyArity f args) stack heap
   | Fun vars exp <- lookupHeap f heap, length args > length vars = do
        setRule "CALLK"
        let (enoughArgs, restArgs) = splitAt (length vars) args
            newExp = subs (mkSubList $ zip vars enoughArgs) exp
        return $ Just (newExp, (ApplyToArgs restArgs) : stack, heap)
-- PAP2
smallStep EvalApply (FunApp _anyArity f args) stack heap
   | Fun vars exp <- lookupHeap f heap, length args < length vars = do
        setRule "PAP2"
        p <- freshVar
        let newHeap = updateHeap p (Pap f args) heap
        return $ Just (Atom (Variable p), stack, newHeap)
-- TCALL
-- XXX fix up call stack?
smallStep EvalApply (FunApp Nothing f args) stack heap
   | Thunk exp thunkCallStack <- lookupHeap f heap = do
        setRule "TCALL"
        return $ Just (Atom (Variable f), (ApplyToArgs args) : stack, heap)
-- PCALL
smallStep EvalApply (FunApp _anyArity f args) stack heap
   | Pap g papArgs <- lookupHeap f heap = do
        setRule "PCALL"
        return $ Just (FunApp Nothing g (papArgs ++ args), stack, heap)
-- RETFUN
smallStep EvalApply (Atom (Variable f)) (ApplyToArgs args : stack) heap
   | object <- lookupHeap f heap, isFun object || isPap object = do
        setRule "RETFUN"
        return $ Just (FunApp Nothing f args, stack, heap)

-- NOTHING MORE TO DO
smallStep _anyStyle _other _stack _heap = do
   setRule "None"
   return Nothing

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

-- | Convenience function for making integer primitives. 
mkIntPrim :: (Integer -> Integer -> Integer) -> [Atom] -> Stack -> Heap -> Eval (Atom, Stack, Heap)
mkIntPrim op [Literal (Integer i), Literal (Integer j)] stack heap
   = return (Literal $ Integer (i `op` j), stack, heap)

-- | Convenience function for making integer comparison primitives. 
mkIntComparePrim :: (Integer -> Integer -> Bool) -> [Atom] -> Stack -> Heap -> Eval (Atom, Stack, Heap)
mkIntComparePrim op args stack heap = mkIntPrim (\i j -> if i `op` j then 1 else 0) args stack heap

setThunkStack :: CallStack -> Object -> Object
setThunkStack cs (Thunk e _oldCS) = Thunk e cs
setThunkStack cs other = other

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
   subs _s l@(Literal {}) = l

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
   subs s (Stack str e) = Stack str $ subs s e

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
   subs s (Thunk exp cs) = Thunk (subs s exp) cs
   subs _s BlackHole = BlackHole
   subs _s Error = Error
