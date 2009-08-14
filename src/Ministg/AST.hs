-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.AST 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- The representation of the abstract syntax tree for ministg programs. 
-----------------------------------------------------------------------------
module Ministg.AST where

import Prelude 
import Ministg.Lexer

-- | Variables (also known as identifiers).
type Var = String
-- | Data constructor names.
type Constructor = String

-- | Literal integers. These correspond to unboxed integers in the semantics.
data Literal = Integer Integer 
   deriving (Eq, Show)

-- | Atomic expressions.
data Atom
   = Literal Literal    -- ^ Literal values (unoboxed integers).
   | Variable Var       -- ^ Variables.
   deriving (Eq, Show)

-- | The arity (number of parameters) of a function. It is only known when the function 
-- being applied is statically known (not lambda bound).
type FunArity = Maybe Int

-- | Expressions.
data Exp 
   = Atom Atom                  -- ^ Atomic expressions (literals, variables).
   | FunApp FunArity Var [Atom] -- ^ Function application (f^k a_1 ... a_n, n >= 1).
   | PrimApp Prim [Atom]        -- ^ Saturated primitive application (op a_1 ... a_n, n >= 1).
   | Let Var Object Exp         -- ^ Let declaration. 
   | Case Exp [Alt]             -- ^ Case expression.
   | Error                      -- ^ Raise an exception.
   | Stack String Exp           -- ^ Like SCC, but just for stacks. (stack str (exp))
   deriving (Eq, Show)

-- | Case alternatives (the right-hand-sides of case branches).
data Alt
   = PatAlt Constructor [Var] Exp  -- ^ Constructor pattern (C x_1 ... x_n -> e, n >= 0).
   | DefaultAlt Var Exp            -- ^ Default pattern (matches anything) (x -> e).
   deriving (Eq, Show)

-- | Objects. These serve two roles in the language: 
-- 
-- (1) as part of the language syntax (except blackholes).
-- (2) as things which are allocated on the heap during execution.

data Object
   = Fun [Var] Exp                 -- ^ Function values (FUN (x_1 ... x_n -> e).
   | Pap Var [Atom]                -- ^ Partial applications (PAP (f a_1 ... a_n)).
   | Con Constructor [Atom]        -- ^ Data constructor application (CON (C a_1 ... a_n)).
   | Thunk Exp                     -- ^ THUNK (e).
   | BlackHole                     -- ^ BLACKHOLE (only during evaluation - not part of the language syntax).
   deriving (Eq, Show)

-- | A top-level declaration (f = obj).
type Decl = (Var, Object)
-- | A whole program.
type Program = [Decl]

-- | Primitive operators.
data Prim
   = Add                        -- ^ Unboxed integer addition (x + y).
   | Subtract                   -- ^ Unboxed integer subtraction (x - y).
   | Multiply                   -- ^ Unboxed integer multiplication (x * y).
   | Equality                   -- ^ Unboxed integer equality test (x == y).
   | LessThan                   -- ^ Unboxed integer less-than comparison (x < y).
   | GreaterThan                -- ^ Unboxed integer greater-than comparison ( x > y). 
   | LessThanEquals             -- ^ Unboxed integer less-than-equals comparison ( x <= y).
   | GreaterThanEquals          -- ^ Unboxed integer greater-than-equals comparison ( x >= y).
   | IntToBool                  -- ^ Convert an unboxed integer to a (boxed) boolean ( 1 = True, 0 = False).
   deriving (Eq, Show)
