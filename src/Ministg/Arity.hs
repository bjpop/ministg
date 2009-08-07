{-# OPTIONS_GHC -XTypeSynonymInstances #-}
module Ministg.Arity (runArity, Arity) where

import Data.Map as Map
import Control.Monad.Reader
import Control.Applicative
import Ministg.AST
import Data.List (foldl')

type ArityMap = Map Var Int
type A a = Reader ArityMap a

runArity :: Arity a => a -> a
runArity x = runReader (arity x) Map.empty 

class Arity a where
   arity :: a -> A a 

instance Arity Alt where
   arity (PatAlt con args body) = 
      PatAlt con args <$> local (clearVars args) (arity body)
   arity (DefaultAlt var body) = 
      DefaultAlt var <$> local (clearVars [var]) (arity body)

instance Arity Object where
   arity (Fun args body) = Fun args <$> local (clearVars args) (arity body)
   arity (Thunk exp) = Thunk <$> arity exp 
   arity other = return other

instance Arity Program where
   arity decls = local (Map.union as) $ mapM arity decls
      where
      as :: ArityMap
      as = Map.fromList [ (var, countArgs obj) | (var, obj) <- decls, isFun obj]

isFun :: Object -> Bool
isFun (Fun {}) = True
isFun other = False

countArgs :: Object -> Int
countArgs (Fun args _) = length args
countArgs other = error $ "countArgs called on non function: " ++ show other

instance Arity Decl where
   arity (var, object) = (,) var <$> arity object

instance Arity Exp where
   arity (FunApp _oldArity var args) = 
      FunApp <$> asks (Map.lookup var) <*> pure var <*> pure args
   arity (Let var object exp)
      | isFun object = 
           Let var <$> arity object <*> local (Map.insert var $ countArgs object) (arity exp)
      | otherwise = Let var <$> arity object <*> local (clearVars [var]) (arity exp)
   arity (Case exp alts) = Case <$> arity exp <*> mapM arity alts 
   arity other = return other

clearVars :: [Var] -> ArityMap -> ArityMap
clearVars vars map = foldl' (flip Map.delete) map vars
