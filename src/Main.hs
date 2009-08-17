-----------------------------------------------------------------------------
-- |
-- Module      : Main 
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- The main module of ministg. An interpreter for the operational semantics
-- of the STG machine, as set out in the "How to make a fast curry" paper
-- by Simon Marlow and Simon Peyton Jones.
-----------------------------------------------------------------------------

module Main where

import Ministg.AST (Program, prettyProgram)
import Ministg.Parser (parser)
import Ministg.Lexer (lexer, Token)
import Control.Monad (when, unless)
import System (getArgs, exitFailure)
import System.Directory (doesDirectoryExist, createDirectory)
import Ministg.Utils (safeReadFile)
import Ministg.Arity (runArity)
import Ministg.Eval (run)
import Ministg.Pretty (render)
import Ministg.Options (processOptions, Flag (..), Dumped (..), existsFlag, getTraceDir)

-- | The main driver of the program.
main :: IO ()
main = do
   args <- getArgs
   (flags, [file]) <- processOptions args
   tryContents <- safeReadFile file
   case tryContents of
      Left error -> putStrLn error 
      Right contents -> do
         -- parse the program
         program <- parseFile file contents 
         dump flags DumpAST (show program) "The AST of the input program:\n"
         dump flags DumpParsed (render $ prettyProgram program) 
                    "The parsed program:\n"
         -- compute arities of known functions
         let arityProgram = runArity program
         dump flags DumpArity (render $ prettyProgram arityProgram) 
                    "The program after arity analysis:\n"
         -- create trace directory if necessary
         when (existsFlag flags Trace) $ do
             let traceDir = getTraceDir flags
             dirExist <- doesDirectoryExist traceDir
             unless dirExist $ createDirectory traceDir 
         -- interpret the program
         run flags arityProgram

dump :: [Flag] -> Dumped -> String -> String -> IO () 
dump flags dumped str msg = 
   when (existsFlag flags $ Dump dumped) $ do
      putStrLn msg 
      putStrLn str 
      putStr "\n"

-- | Parse a ministg program from the contents of a named file.
parseFile :: FilePath   -- ^ The name of the file.
          -> String     -- ^ The contents of the file. 
          -> IO Program -- ^ The parsed program.
parseFile file contents 
   = case parser file contents of
       Left e -> (putStrLn $ "Parse error: " ++ show e) >> exitFailure
       Right prog -> return prog 
