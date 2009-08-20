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
-- import qualified Ministg.Lexer as Lex
import Ministg.CallStack (CallStack, prettyCallStack)
import Ministg.Pretty
import Data.Set as Set hiding (map)

-- | Variables (also known as identifiers).
type Var = String
-- | Data constructor names.
type Constructor = String

class FreeVars t where
   freeVars :: t -> Set Var

instance FreeVars t => FreeVars [t] where
   freeVars = Set.unions . map freeVars

-- | Literal integers. These correspond to unboxed integers in the semantics.
data Literal = Integer Integer 
   deriving (Eq, Show)

instance Pretty Literal where
   pretty (Integer i) = pretty i

-- | Atomic expressions.
data Atom
   = Literal Literal    -- ^ Literal values (unoboxed integers).
   | Variable Var       -- ^ Variables.
   deriving (Eq, Show)

instance Pretty Atom where
   pretty (Literal l) = pretty l
   pretty (Variable v) = text v

instance FreeVars Atom where
   freeVars (Literal {}) = Set.empty
   freeVars (Variable v) = Set.singleton v

-- | Is an atom a literal?
isLiteral :: Atom -> Bool
isLiteral (Literal {}) = True
isLiteral _other = False

-- | The arity (number of parameters) of a function. It is only known when the function 
-- being applied is statically known (not lambda bound).
type FunArity = Maybe Int

prettyArity :: FunArity -> Doc
prettyArity Nothing = text "_?" 
prettyArity (Just i) = text "_" <> int i

-- | Expressions.
data Exp 
   = Atom Atom                  -- ^ Atomic expressions (literals, variables).
   | FunApp FunArity Var [Atom] -- ^ Function application (f^k a_1 ... a_n, n >= 1).
   | PrimApp Prim [Atom]        -- ^ Saturated primitive application (op a_1 ... a_n, n >= 1).
   | Let Var Object Exp         -- ^ Let declaration. 
   | Case Exp [Alt]             -- ^ Case expression.
   | Stack String Exp           -- ^ Like SCC, but just for stacks. (stack str (exp))
   deriving (Eq, Show)

instance FreeVars Exp where
   freeVars (Atom a) = freeVars a
   freeVars (FunApp _arity var args) = Set.singleton var `Set.union` freeVars args
   freeVars (PrimApp prim args) = freeVars args
   -- Treat this as a letrec, which means that the var is bound (not free) in the object
   freeVars (Let var object exp) 
      = Set.delete var (freeVars exp `Set.union` freeVars object) 
   freeVars (Case exp alts)
      = freeVars exp `Set.union` freeVars alts
   freeVars (Stack _str exp) = freeVars exp

instance Pretty Exp where
   pretty (Atom a) = pretty a
   pretty (FunApp arity var atoms) = text var <> prettyArity arity <+> hsep (map pretty atoms)
   pretty (PrimApp prim atoms) = pretty prim <+> hsep (map pretty atoms)
   pretty letExp@(Let var obj exp) 
      = maybeNest (text "let {") prettyDecls (rbrace <+> text "in" <+> pretty inExp)
      where
      (decls, inExp) = unflattenLet letExp
      prettyDecls = vcat (punctuate semi (map pretty decls))
      maybeNest letPart declPart inPart
         | length decls < 2 = letPart <+> declPart <+> inPart
         | otherwise = letPart $$ (nest 3 declPart) $$ inPart
   pretty (Case exp alts) = 
      text "case" <+> pretty exp <+> text "of {" $$ 
      nest 3 (vcat (punctuate semi (map pretty alts))) $$
      rbrace
   pretty (Stack annotation exp) = 
      maybeNest exp (text "stack" <+> doubleQuotes (text annotation)) (pretty exp)

isNestedExp :: Exp -> Bool
isNestedExp (Let {}) = True
isNestedExp (Case {}) = True
isNestedExp (Stack {}) = True
isNestedExp other = False 

unflattenLet :: Exp -> ([Decl], Exp)
unflattenLet exp = unflattenLetAcc exp []
   where
   unflattenLetAcc :: Exp -> [Decl] -> ([Decl], Exp)
   unflattenLetAcc (Let var obj exp) ds = unflattenLetAcc exp (Decl var obj : ds) 
   unflattenLetAcc exp ds = (reverse ds, exp) 

-- | Case alternatives (the right-hand-sides of case branches).
data Alt
   = PatAlt Constructor [Var] Exp  -- ^ Constructor pattern (C x_1 ... x_n -> e, n >= 0).
   | DefaultAlt Var Exp            -- ^ Default pattern (matches anything) (x -> e).
   deriving (Eq, Show)

instance FreeVars Alt where
   freeVars (PatAlt constructor args exp) = freeVars exp \\ Set.fromList args
   freeVars (DefaultAlt var exp) = Set.delete var $ freeVars exp 

instance Pretty Alt where
   pretty (PatAlt con vars exp) = maybeNest exp (text con <+> hsep (map text vars) <+> rightArrow) (pretty exp)
   pretty (DefaultAlt var exp) = text var <+> rightArrow <+> pretty exp

rightArrow :: Doc
rightArrow = text "->" 

-- | Objects. These serve two roles in the language: 
-- 
-- (1) as part of the language syntax (except blackholes).
-- (2) as things which are allocated on the heap during execution.

data Object
   = Fun [Var] Exp                 -- ^ Function values (FUN (x_1 ... x_n -> e).
   | Pap Var [Atom]                -- ^ Partial applications (PAP (f a_1 ... a_n)).
   | Con Constructor [Atom]        -- ^ Data constructor application (CON (C a_1 ... a_n)).
   | Thunk Exp CallStack           -- ^ THUNK (e).
   | BlackHole                     -- ^ BLACKHOLE (only during evaluation - not part of the language syntax).
   | Error                         -- ^ Raise an exception.
   deriving (Eq, Show)

instance FreeVars Object where
   freeVars (Fun vars exp) = freeVars exp \\ Set.fromList vars
   freeVars (Pap var args) = Set.singleton var `Set.union` freeVars args
   freeVars (Con constructor args) = freeVars args
   freeVars (Thunk exp callStack) = freeVars exp 
   freeVars BlackHole = Set.empty
   freeVars Error = Set.empty

maybeNest :: Exp -> Doc -> Doc -> Doc
maybeNest exp d1 d2 = if isNestedExp exp then d1 $$ (nest 3 d2) else d1 <+> d2

instance Pretty Object where
   pretty (Fun vars exp) 
      = text "FUN" <> parens (maybeNest exp (hsep (map text vars) <+> rightArrow) (pretty exp))
   pretty (Pap var atoms) = text "PAP" <> parens (text var <+> hsep (map pretty atoms))
   pretty (Con constructor atoms) = text "CON" <> parens (text constructor <+> hsep (map pretty atoms))
   pretty (Thunk exp callStack) 
      = text "THUNK" <> parens (pretty exp) $$ (nest 3 (prettyCallStack callStack))
   pretty BlackHole = text "BLACKHOLE"
   pretty Error = text "ERROR"

-- | Test for "value" objects.
isValue :: Object -> Bool
isValue (Fun {}) = True
isValue (Pap {}) = True
isValue (Con {}) = True
isValue _other = False

-- | Test for FUN objects
isFun :: Object -> Bool
isFun (Fun {}) = True
isFun other = False

-- | Test for PAP objects
isPap :: Object -> Bool
isPap (Pap {}) = True
isPap other = False

-- | A top-level declaration (f = obj).
data Decl = Decl Var Object
   deriving Show

instance Pretty Decl where
   pretty (Decl var obj) = text var <+> equals <+> pretty obj

-- | A whole program.
newtype Program = Program [Decl]
   deriving Show

instance Pretty Program where
   pretty (Program decls) = vcat (punctuate semi (map pretty decls))

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

instance Pretty Prim where
   pretty Add = text "plus#"
   pretty Subtract = text "sub#"
   pretty Multiply = text "mult#"
   pretty Equality = text "eq#"
   pretty LessThan = text "lt#"
   pretty GreaterThan = text "gt#"
   pretty LessThanEquals = text "lte#"
   pretty GreaterThanEquals = text "gte#" 
   pretty IntToBool = text "intToBool#"
