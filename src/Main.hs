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

import Ministg.AST
   ( Program, prettyProgram )

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

import Ministg.Eval (run, Style(PushEnter, EvalApply))

import Ministg.Pretty ( render )

-- | The main driver of the program.
main :: IO ()
main = do
   args <- getArgs
   when (length args > 0) $ do
      let file = head args 
      tryContents <- safeReadFile file
      case tryContents of
         Left error -> putStrLn error 
         Right contents -> do
            -- parse the program
            program <- parseFile file contents 
            -- print program
            -- compute arities of known functions
            let arityProgram = runArity program
            -- run PushEnter arityProgram
            putStrLn $ render $ prettyProgram $ arityProgram

-- | Parse a ministg program from the contents of a named file.
parseFile :: FilePath   -- ^ The name of the file.
          -> String     -- ^ The contents of the file. 
          -> IO Program -- ^ The parsed program.
parseFile file contents 
   = case parser file contents of
       Left e -> do putStrLn $ "Parse error: " ++ show e
                    exitFailure
       Right prog -> return prog 
