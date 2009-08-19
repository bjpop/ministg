{-# OPTIONS_GHC -XTypeSynonymInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.Arity
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Arity analysis of ministg programs: compute how many arguments each
-- top-level and let-bound function has, and annotate the application sites
-- of those functions.
-----------------------------------------------------------------------------

module Ministg.Arity (runArity, Arity) where

import Data.Map as Map
import Control.Monad.Reader
import Control.Applicative
import Ministg.AST
import Data.List (foldl')

-- | A mapping from variable names (names of functions) to their respective
-- arities.
type ArityMap = Map Var Int
-- | A monad for pushing arity information down the AST, taking care of 
-- variable scope. 
type A a = Reader ArityMap a

-- | Arity analysis of a program fragment.
runArity :: Arity a => a -> a
runArity x = runReader (arity x) Map.empty 

-- | Overloaded arity function.
class Arity a where
   arity :: a -> A a 

instance Arity Alt where
   arity (PatAlt con args body) = 
      PatAlt con args <$> local (clearVars args) (arity body)
   arity (DefaultAlt var body) = 
      DefaultAlt var <$> local (clearVars [var]) (arity body)

instance Arity Object where
   arity (Fun args body) = Fun args <$> local (clearVars args) (arity body)
   arity (Thunk exp cs) = Thunk <$> arity exp <*> pure cs
   arity other = return other

instance Arity Program where
   arity (Program decls) = Program <$> (local (Map.union as) $ mapM arity decls)
      where
      as :: ArityMap
      as = Map.fromList [ (var, countArgs obj) | Decl var obj <- decls, isFun obj]

-- | True if an object is a funciton (FUN).
isFun :: Object -> Bool
isFun (Fun {}) = True
isFun other = False

-- | Count the number of arguments (really parameters) of a function object).
countArgs :: Object -> Int
countArgs (Fun args _) = length args
countArgs other = error $ "countArgs called on non function: " ++ show other

instance Arity Decl where
   arity (Decl var object) = Decl var <$> arity object

instance Arity Exp where
   arity (FunApp _oldArity var args) = 
      FunApp <$> asks (Map.lookup var) <*> pure var <*> pure args
   arity (Let var object exp)
      | isFun object = 
           Let var <$> arity object <*> local (Map.insert var $ countArgs object) (arity exp)
      | otherwise = Let var <$> arity object <*> local (clearVars [var]) (arity exp)
   arity (Case exp alts) = Case <$> arity exp <*> mapM arity alts 
   arity (Stack annotation exp) = Stack annotation <$> arity exp
   arity exp@(Atom {}) = return exp
   arity exp@(PrimApp {}) = return exp

-- | Remove a list of variables from an ArityMap.
clearVars :: [Var] -> ArityMap -> ArityMap
clearVars vars map = foldl' (flip Map.delete) map vars
