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

newtype Token = Token (SourcePos, Symbol)

instance Show Token where
   show (Token (pos, symbol)) = show symbol

type Ident = String

data Symbol
   = Variable Ident 
   | Constructor Ident
   | Natural Integer
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
   | PrintInt
   | IntToBool
   deriving (Eq, Show)

lexer :: String -> String -> Either ParseError [Token] 
lexer = parse tokenise 

tokenise :: Parser [Token]
tokenise = do
   skip 
   ts <- sepEndBy token skip
   eof
   return ts

skip :: Parser ()
skip = skipMany (comment <|> whiteSpace)

whiteSpace :: Parser ()
whiteSpace = space >> return ()

comment :: Parser () 
comment = singleLineComment

singleLineComment :: Parser ()
singleLineComment = do
   string  "#" 
   manyTill anyChar eol 
   return ()

eol :: Parser ()
eol = newline >> return ()

token :: Parser Token
token = 
   punctuation       <|>
   keyword           <|> 
   variable          <|> 
   constructor       <|>
   parenthesis       <|> 
   number

number :: Parser Token
number = tokenPos parseNum Natural
   where
   parseNum :: Parser Integer
   parseNum = do
      ds <- many1 digit
      return $ read ds

variable :: Parser Token
variable = tokenPos (parseIdent lower) Variable 

constructor :: Parser Token
constructor = tokenPos (parseIdent upper) Constructor

parseIdent :: Parser Char -> Parser String
parseIdent firstChar = do
   start <- firstChar 
   let identChar = char '_' <|> alphaNum
   rest  <- many identChar
   return $ start : rest

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
   key "plus" Plus <|>
   key "sub" Minus <|>
   key "mult" Times <|>
   key "eq" Equality <|>
   key "lt" LessThan <|>
   key "lte" LessThanEquals <|>
   key "gt" GreaterThan <|>
   key "gte" GreaterThanEquals <|>
   key "printInt" PrintInt <|>
   key "intToBool" IntToBool
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
tokenPos parser mkToken = do
   pos <- getPosition
   res <- parser 
   return $ Token (pos, mkToken res)
