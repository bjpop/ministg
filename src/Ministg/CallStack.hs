{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.CallStack
-- Copyright   : (c) 2009-2012 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Stack of program annotations. Simulate a call stack.
-----------------------------------------------------------------------------

module Ministg.CallStack (CallStack, push, showCallStack, prettyCallStack) where

import Ministg.Pretty
#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

type CallStack = [String]

push :: String -> CallStack -> CallStack
push = (:)

showCallStack :: CallStack -> String
showCallStack = unlines

prettyCallStack :: CallStack -> Doc
prettyCallStack [] = empty
prettyCallStack stack = char '<' <> hcat (punctuate (text "|") (map text stack)) <> char '>'
