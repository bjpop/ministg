module Main where

import Ministg.AST
   ( Program )

import Ministg.Parser
   ( parser )

import Ministg.Lexer (lexer, Token)

import Control.Monad
   ( when )

import System
   ( getArgs
   , exitFailure
   )

import Ministg.Utils
   ( safeReadFile )

import Ministg.Arity
   ( runArity )

import Ministg.Eval (run)

main :: IO ()
main = do
   args <- getArgs
   when (length args > 0) $ do
      let file = head args 
      tryContents <- safeReadFile file
      case tryContents of
         Left error -> putStrLn error 
         Right contents -> do
            program <- parseFile file contents 
            let arityProgram = runArity program
            -- print arityProgram 
            run arityProgram

parseFile :: FilePath -> String -> IO Program 
parseFile file contents 
   = case parser file contents of
       Left e -> do putStrLn $ "Parse error: " ++ show e
                    exitFailure
       Right prog -> return prog 
