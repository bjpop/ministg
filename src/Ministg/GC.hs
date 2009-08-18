-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.GC
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Garbage collection for ministg.
-----------------------------------------------------------------------------
module Ministg.GC where

import Data.Set as Set hiding (map)
import Data.Map as Map hiding (map, fold)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets)
import Ministg.Pretty
import Ministg.State
import Ministg.AST

garbageCollect :: Exp -> Stack -> Heap -> Eval Heap
garbageCollect exp stack heap = do
   wantGC <- gets state_gc 
   if wantGC 
      then return $ collect roots heap Map.empty 
      else return heap
   where
   roots = freeVars exp `Set.union` freeVars stack

collect :: Set Var -> Heap -> Heap -> Heap
collect vars oldHeap newHeap
   = collector vars newHeap
   where
   collector vars newHeap 
      | Set.null vars = newHeap
      | otherwise = collector newVars nextHeap 
      where
      (newVars, nextHeap) = fold collectVar (Set.empty, newHeap) vars 
      collectVar :: Var -> (Set Var, Heap) -> (Set Var, Heap)
      collectVar var (vars, heap)
         | Map.member var heap = (vars, heap)
         | otherwise = (newVars, newHeap)
         where
         object = lookupHeap var oldHeap 
         newVars = freeVars object `Set.union` vars
         newHeap = updateHeap var object heap
