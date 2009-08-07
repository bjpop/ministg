module Ministg.AST where

import Prelude 
import Ministg.Lexer

type Var = String
type Constructor = String

data Literal = Integer Integer -- literal integer
   deriving (Eq, Show)

data Atom
   = Literal Literal
   | Variable Var
   deriving (Eq, Show)

type FunArity = Maybe Int

data Exp 
   = Atom Atom            -- atoms (literals, variables)
   | FunApp FunArity Var [Atom]    -- function application
   | PrimApp Prim [Atom]   -- primitive application
   | Let Var Object Exp   -- let declaration (non recursive) 
   | Case Exp [Alt]       -- case expression
   deriving (Eq, Show)

data Prim
   = Add | Subtract | Multiply | Equality | LessThan | 
     GreaterThan | LessThanEquals | GreaterThanEquals | PrintInt | IntToBool
   deriving (Eq, Show)

data Alt
   = PatAlt Constructor [Var] Exp  -- C x_1 .. x_n -> e
   | DefaultAlt Var Exp            -- x -> e
   deriving (Eq, Show)

data Object
   = Fun [Var] Exp
   | Pap Var [Atom]
   | Con Constructor [Atom]
   | Thunk Exp
   | BlackHole
   deriving (Eq, Show)

type Decl = (Var, Object)
type Program = [Decl]
