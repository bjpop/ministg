-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.Lexer
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Lexical analysis for ministg programs.
-----------------------------------------------------------------------------

module Ministg.Lexer 
   ( Token (..)
   , Symbol (..)
   , Ident
   , lexer
   )
where

import Data.Char 
   ( isDigit
   , isAlpha
   , isPrint
   , isLower
   )

import Text.ParserCombinators.Parsec hiding (token)
import Control.Applicative hiding ((<|>), many)

newtype Token = Token (SourcePos, Symbol)

instance Show Token where
   show (Token (pos, symbol)) = show symbol

type Ident = String

data Symbol
   = Variable Ident 
   | Constructor Ident
   | Natural Integer
   | QuotedString String
   | Equals
   | BackSlash 
   | RightArrow
   | Let
   | In 
   | Case
   | Of
   | LeftParen
   | RightParen
   | LeftBrace
   | RightBrace
   | SemiColon
   | Fun
   | Con
   | Pap
   | Thunk 
   | Plus
   | Minus
   | Times
   | Equality
   | GreaterThan
   | LessThan
   | GreaterThanEquals
   | LessThanEquals
   | IntToBool
   | Stack
   | Error
   deriving (Eq, Show)

lexer :: String -> String -> Either ParseError [Token] 
lexer = parse tokenise 

tokenise :: Parser [Token]
tokenise = skip *> sepEndBy token skip <* eof

skip :: Parser ()
skip = skipMany (comment <|> whiteSpace)

whiteSpace :: Parser ()
whiteSpace = space >> return ()

comment :: Parser () 
comment = singleLineComment

singleLineComment :: Parser ()
singleLineComment = string  "#" >> manyTill anyChar eol >> return ()

eol :: Parser ()
eol = newline >> return ()

-- XXX Perhaps it is not sensible to divide the tokens based on their
-- syntactic role. Sometimes tokens from different syntactic classes can have
-- the same prefix.
token :: Parser Token
token = 
   punctuation <|>
   keyword     <|> 
   variable    <|> 
   constructor <|>
   parenthesis <|> 
   number <|>
   quotedString

number :: Parser Token
number = tokenPos parseNum Natural
   where
   parseNum :: Parser Integer
   parseNum = read <$> many1 digit

quotedString :: Parser Token
quotedString = tokenPos parseString QuotedString
   where
   parseString :: Parser String
   parseString = char '"' *> manyTill anyChar (char '"')

variable :: Parser Token
variable = tokenPos (parseIdent lower) Variable 

constructor :: Parser Token
constructor = tokenPos (parseIdent upper) Constructor

parseIdent :: Parser Char -> Parser String
parseIdent firstChar = (:) <$> firstChar <*> many (char '_' <|> alphaNum) 

keyword :: Parser Token
keyword =
   key "let"   Let   <|>
   key "in"    In    <|>
   key "case"  Case  <|>
   key "of"    Of    <|>
   key "FUN"   Fun   <|>
   key "CON"   Con   <|>
   key "PAP"   Pap   <|>
   key "THUNK" Thunk <|>
   key "plus#" Plus  <|>
   key "sub#"  Minus <|>
   key "mult#" Times <|>
   key "eq#"   Equality <|>
   key "lt#"   LessThan <|>
   key "lte#"  LessThanEquals <|>
   key "gt#"   GreaterThan <|>
   key "gte#"  GreaterThanEquals <|>
   key "intToBool#" IntToBool <|>
   key "ERROR" Error <|>
   key "stack" Stack
   where
   key :: String -> Symbol -> Parser Token
   key str keyWord = tokenPos (try kwParser) (const keyWord)
      where
      kwParser = string str >> notFollowedBy alphaNum

parenthesis :: Parser Token
parenthesis = 
   simpleTok "(" LeftParen  <|> 
   simpleTok ")" RightParen <|>
   simpleTok "{" LeftBrace  <|>
   simpleTok "}" RightBrace  

punctuation :: Parser Token
punctuation = 
   simpleTok "=" Equals 
   <|> simpleTok ";" SemiColon 
   <|> simpleTok "\\" BackSlash 
   <|> simpleTok "->" RightArrow

simpleTok :: String -> Symbol -> Parser Token
simpleTok str token = tokenPos (string str) (const token) 

tokenPos :: Parser a -> (a -> Symbol) -> Parser Token
tokenPos parser mkToken = 
  Token <$> ((,) <$> getPosition <*> (mkToken <$> parser))
