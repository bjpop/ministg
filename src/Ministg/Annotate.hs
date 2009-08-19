-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.Annotate
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Add stack annotations to top-level functions in ministg programs. 
-----------------------------------------------------------------------------
module Ministg.Annotate where

import Ministg.AST

class Annotate t where
   annotate :: t -> t

instance Annotate a => Annotate [a] where
   annotate = map annotate

instance Annotate Program where
   annotate (Program decls) = Program $ annotate decls

instance Annotate Decl where
   -- don't annotate functions which are already annotated (by the user)
   annotate decl@(Decl _ (Fun _ (Stack {}))) = decl
   annotate (Decl var (Fun args body))
      = Decl var (Fun args (Stack var body))
   -- don't annotate thunks which are already annotated (by the user)
   annotate decl@(Decl _ (Thunk (Stack {}) _)) = decl
   annotate decl@(Decl var (Thunk body callStack)) 
      = Decl var (Thunk (Stack var body) callStack) 
   annotate other = other
