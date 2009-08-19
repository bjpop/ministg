-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.Parser
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Parsing for ministg programs.
-----------------------------------------------------------------------------
module Ministg.Parser 
   ( parser )
   where

import Prelude hiding (exp, subtract)
import Text.ParserCombinators.Parsec hiding (Parser)
import qualified Ministg.Lexer as Lex
import Ministg.AST hiding (rightArrow)
import Control.Applicative hiding ((<|>), many)

type Parser a = GenParser Lex.Token () a

tokenParser :: (Lex.Symbol -> Maybe a) -> Parser a
tokenParser test
   = token showToken posToken testToken
   where
   showToken (Lex.Token (pos,tok)) = show tok
   posToken  (Lex.Token (pos,tok)) = pos
   testToken (Lex.Token (pos,tok)) = test tok

symbol :: Lex.Symbol -> Parser ()
symbol tok
   = tokenParser $ 
        \next -> if next == tok then Just () else Nothing

parser :: FilePath -> String -> Either ParseError Program 
parser source input
   = case Lex.lexer source input of
        Left err -> Left err
        Right toks -> parse program source toks 

program :: Parser Program
program = Program <$> sepEndBy decl semiColon <* eof

decl :: Parser Decl
decl = Decl <$> var <*> (equals *> object)

exp :: Parser Exp
exp = funCallOrVar <|> 
      expAtomLiteral <|> 
      primApp <|> 
      letExp <|> 
      caseExp <|>
      stack

stack :: Parser Exp
stack = Stack <$> (symbol Lex.Stack *> quotedString) <*> (leftParen *> exp <* rightParen)

quotedString :: Parser String 
quotedString = tokenParser getString
   where
   getString (Lex.QuotedString s) = Just s
   getString other = Nothing 

funCallOrVar :: Parser Exp
funCallOrVar = do
   v <- var
   args <- many atom
   return $ if null args
      then Atom $ Variable v
      -- we don't know the arity of the function yet.
      else FunApp Nothing v args 

expAtomLiteral :: Parser Exp
expAtomLiteral = Atom <$> atomLiteral 

expAtom :: Parser Exp
expAtom = Atom <$> atom

primApp :: Parser Exp
primApp = PrimApp <$> primOp <*> many1 atom 

primOp, add, subtract, multiply, eq, lessThan, greaterThan, lessThanEquals, greaterThanEquals :: Parser Prim

primOp = 
   add <|> 
   subtract <|> 
   multiply <|> 
   eq <|> 
   lessThan <|> 
   greaterThan <|> 
   lessThanEquals <|> 
   greaterThanEquals <|> 
   intToBool

add = const Add <$> symbol Lex.Plus
subtract = const Subtract <$> symbol Lex.Minus
multiply = const Multiply <$> symbol Lex.Times
eq = const Equality <$> symbol Lex.Equality
lessThan = const LessThan <$> symbol Lex.LessThan
lessThanEquals = const LessThanEquals <$> symbol Lex.LessThanEquals
greaterThan = const GreaterThan <$> symbol Lex.GreaterThan
greaterThanEquals = const GreaterThanEquals <$> symbol Lex.GreaterThanEquals
intToBool = const IntToBool <$> symbol Lex.IntToBool

letExp :: Parser Exp
letExp = flattenLet <$> (symbol Lex.Let *> leftBrace *> sepEndBy1 decl semiColon <* rightBrace) <*> (symbol Lex.In *> exp)

flattenLet :: [Decl] -> Exp -> Exp
flattenLet [Decl var obj] body = Let var obj body
flattenLet (Decl var obj : decls) body = Let var obj $ flattenLet decls body 

caseExp :: Parser Exp
caseExp = Case <$> (symbol Lex.Case *> exp) <*> (symbol Lex.Of *> leftBrace *> sepEndBy1 alt semiColon <* rightBrace)

atom, atomLiteral, atomVariable :: Parser Atom
atom = atomLiteral <|> atomVariable
atomLiteral = Literal <$> literal
atomVariable = Variable <$> var

literal :: Parser Literal
literal = Integer <$> natural 

alt :: Parser Alt
alt = patAlt <|> defaultAlt

patAlt :: Parser Alt
patAlt = PatAlt <$> constructor <*> many var <*> (rightArrow *> exp)

defaultAlt :: Parser Alt
defaultAlt = DefaultAlt <$> var <*> (rightArrow *> exp)

object :: Parser Object
object = fun <|> pap <|> con <|> thunk <|> errorObj

fun, pap, con, thunk, errorObj  :: Parser Object
fun = Fun <$> (symbol Lex.Fun *> leftParen *> many1 var) <*> (rightArrow *> exp <* rightParen)
pap = Pap <$> (symbol Lex.Pap *> leftParen *> var) <*> (many1 atom <* rightParen)
con = Con <$> (symbol Lex.Con *> leftParen *> constructor) <*> (many atom <* rightParen)
thunk = Thunk <$> (symbol Lex.Thunk *> leftParen *> exp <* rightParen) <*> pure []
errorObj = const Error <$> symbol Lex.Error

var :: Parser Var 
var = tokenParser getIdent
   where
   getIdent (Lex.Variable s) = Just s
   getIdent other = Nothing 

constructor :: Parser Constructor 
constructor = tokenParser getIdent
   where
   getIdent (Lex.Constructor s) = Just s
   getIdent other = Nothing 

equals :: Parser ()
equals = symbol Lex.Equals 

semiColon :: Parser ()
semiColon = symbol Lex.SemiColon 

rightArrow :: Parser ()
rightArrow = symbol Lex.RightArrow

leftParen, rightParen :: Parser ()
leftParen = symbol Lex.LeftParen
rightParen = symbol Lex.RightParen

leftBrace, rightBrace :: Parser ()
leftBrace = symbol Lex.LeftBrace
rightBrace = symbol Lex.RightBrace

natural :: Parser Integer 
natural = 
   tokenParser getNat
   where
   getNat :: Lex.Symbol -> Maybe Integer 
   getNat (Lex.Natural n) = Just n 
   getNat other = Nothing 
