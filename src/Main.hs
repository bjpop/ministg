-----------------------------------------------------------------------------
-- |
-- Module      : Main 
-- Copyright   : (c) 2009-2012 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- The main module of ministg. An interpreter for the operational semantics
-- of the STG machine, as set out in the "How to make a fast curry" paper
-- by Simon Marlow and Simon Peyton Jones.
-----------------------------------------------------------------------------

module Main where

import System.Exit (exitFailure)
import Ministg.AST (Program (Program))
import Ministg.Parser (parser)
import Ministg.Lexer (lexer, Token)
import Control.Monad (when, unless)
import System.Environment (getArgs)
import System.Directory (doesDirectoryExist, createDirectory)
import Ministg.Utils (safeReadFile)
import Ministg.Arity (runArity)
import Ministg.Eval (run)
import Ministg.Pretty (prettyText)
import Ministg.Options (processOptions, Flag (..), Dumped (..), existsFlag, getTraceDir)
import Ministg.Annotate

-- | The main driver of the program.
main :: IO ()
main = do
   args <- getArgs
   (flags, file) <- processOptions args
   -- create trace directory if necessary
   when (existsFlag flags Trace) $ do
      let traceDir = getTraceDir flags
      dirExist <- doesDirectoryExist traceDir
      unless dirExist $ createDirectory traceDir 
   -- parse the file
   Program userProgram <- parseFile flags file
   -- optionally include the Prelude
   fullProgram 
      <- if existsFlag flags NoPrelude
            then return (Program userProgram)
            else do Program preludeProgram <- parseFile flags "Prelude.stg"
                    return $ Program (preludeProgram ++ userProgram)
   -- possibly annotate the program with stack markers
   let annotated = if existsFlag flags Annotate 
                      then annotate fullProgram else fullProgram 
   -- compute arities of known functions
   let arityProgram = runArity annotated 
   dump flags DumpArity (prettyText arityProgram) $
      "The program after arity analysis:\n"
   -- interpret the program
   run flags arityProgram

parseFile :: [Flag] -> FilePath -> IO Program
parseFile flags file = do
   tryContents <- safeReadFile file
   case tryContents of
      Left errorMsg -> do putStrLn $ "Error reading from file: " ++ file
                          putStrLn errorMsg >> exitFailure
      Right contents -> do
         -- parse the program
         case parser file contents of
            Left e -> (putStrLn $ "Parse error: " ++ show e) >> exitFailure
            Right program -> do 
               dump flags DumpAST (show program) $ "The AST of the program from " ++ file ++ ":\n"
               dump flags DumpParsed (prettyText program) $
                    "The parsed program from " ++ file ++ ":\n"
               return program 

dump :: [Flag] -> Dumped -> String -> String -> IO () 
dump flags dumped str msg = 
   when (existsFlag flags $ Dump dumped) $ do
      putStrLn msg 
      putStrLn str 
      putStr "\n"
