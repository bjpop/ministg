-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.DumpState
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Representation of the state of the ministg evaluator.
-----------------------------------------------------------------------------

module Ministg.DumpState (dumpState) where

import Control.Monad.Trans (liftIO)
import Ministg.AST
import Ministg.CallStack (CallStack, push, showCallStack)
import Ministg.Pretty hiding (Style)
import Ministg.State

dumpState :: Exp -> Stack -> Heap -> Eval ()
dumpState exp stack heap = do
   liftIO $ putStrLn $ render $ prettyState exp stack heap

prettyState :: Exp -> Stack -> Heap -> Doc
prettyState exp stack heap
   = text "Expression:" $$ (nest 3 $ pretty exp) $$
     text "Stack:" $$ (nest 3 $ prettyStack stack)

