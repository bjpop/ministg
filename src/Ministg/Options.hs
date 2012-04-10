-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.Options
-- Copyright   : (c) 2009-2012 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Command line option processing. 
-----------------------------------------------------------------------------
module Ministg.Options 
   ( processOptions
   , programName
   , defaultMaxSteps
   , defaultEvalStyle
   , defaultTraceDir
   , Flag (..)
   , EvalStyle (..)
   , Dumped (..)
   , probeFlags
   , probeFlagsFirst
   , existsFlag
   , getTraceDir
   , getMaxTraceSteps
   , getEvalStyle
   )
   where

import System.Console.GetOpt 
import Data.Maybe (fromMaybe)
import Data.Char (toLower, isDigit)
import Data.Maybe (catMaybes)
import System.IO (stderr, hPutStrLn)
import System.Exit

programName :: String
programName = "ministg"

-- This should really come from the cabal file somehow.
versionNumber :: String
versionNumber = "0.2"

versionInfo :: String
versionInfo = unwords [programName, "version", versionNumber]

processOptions :: [String] -> IO ([Flag], String)
processOptions argv = 
   case getOpt RequireOrder options argv of
      (flags, nonOpts, []) 
         | existsFlag flags Help -> do 
              putStrLn $ usageInfo header options
              exitWith ExitSuccess
         | existsFlag flags Version -> do 
              putStrLn versionInfo 
              exitWith ExitSuccess
         | length nonOpts /= 1 ->
              raiseError ["You must specify a single input stg file.\n"]  
         | otherwise -> return (flags, head nonOpts)
      (_, _, errs) -> raiseError errs
      where 
      header = "Usage: " ++ programName ++ " [OPTION...] file"
      failureMsg = programName ++ ": command line error.\n"
      raiseError errs = do
         hPutStrLn stderr $ failureMsg ++ concat errs ++ usageInfo header options
         exitFailure

probeFlags :: [Flag] -> (Flag -> Maybe a) -> [a]
probeFlags flags probe = catMaybes (map probe flags)

probeFlagsFirst :: [Flag] -> (Flag -> Maybe a) -> a -> a
probeFlagsFirst flags probe defaultValue
   | null probed = defaultValue 
   | otherwise = head probed
   where
   probed = probeFlags flags probe

existsFlag :: [Flag] -> Flag -> Bool
existsFlag flags f
   = probeFlagsFirst flags probe False 
   where
   probe someFlag = if f == someFlag then Just True else Nothing 
    
data Flag 
  = Style EvalStyle        -- ^ Which evaluation rules to use (eval/apply or push enter)
  | Trace                  -- ^ Turn tracing on.
  | TraceDir String        -- ^ Directory to save trace file.
  | MaxTraceSteps Integer  -- ^ Maximum trace steps to record.
  | CallStack              -- ^ Include call stack in trace.
  | Dump Dumped            -- ^ Dump something out to debug the interpreter.
  | NoPrelude              -- ^ Do not automatically include the Prelude.
  | NoGC                   -- ^ Disable garbage collection.
  | Help                   -- ^ Print a help message and exit.
  | Version                -- ^ Print the version number. 
  | Annotate               -- ^ Auto annotate the program with stack markers.
  deriving (Eq, Ord, Show)

data EvalStyle
   = EvalApply
   | PushEnter
   deriving (Eq, Ord, Show)

data Dumped
  = DumpAST
  | DumpParsed
  | DumpArity
  | DumpNothing
  deriving (Eq, Ord, Show)

options :: [OptDescr Flag]
options =
 [ Option ['s']     ["style"]     (ReqArg mkStyle "STYLE")   "evaluation STYLE to use (EA = eval apply, PE = push enter)"
 , Option ['t']     ["trace"]     (NoArg Trace)              "record a trace of program evaluation"
 , Option []        ["tracedir"]  (ReqArg TraceDir "DIR")    "directory (DIR) to store trace files"
 , Option ['m']     ["maxsteps"]  (ReqArg mkMaxSteps "STEPS")  "maximum number of evaluation STEPS to record in trace"
 , Option ['c']     ["callstack"] (NoArg CallStack)          "enable call stack tracing"
 , Option []        ["noprelude"] (NoArg NoPrelude)          "do not import the Prelude"
 , Option []        ["nogc"]      (NoArg NoGC)               "disable garbage collector"
 , Option ['d']     ["dump"]      (ReqArg mkDumped "DUMPED") "output DUMPED for debugging purposes (ast, parsed, arity)"
 , Option ['v']     ["version"]   (NoArg Version)            "show version number"
 , Option ['h']     ["help"]      (NoArg Help)               "get help about using this program"
 , Option ['a']     ["annotate"]  (NoArg Annotate)           "automatically annotate the program with stack markers"
 ]

defaultTraceDir :: String
defaultTraceDir = "trace"

defaultEvalStyle :: EvalStyle 
defaultEvalStyle = PushEnter

mkStyle :: String -> Flag
mkStyle = normalMkStyle . map toLower
   where
   normalMkStyle "ea" = Style EvalApply
   normalMkStyle "pe" = Style PushEnter
   normalMkStyle other = Style defaultEvalStyle

mkDumped :: String -> Flag
mkDumped = normalMkDumped . map toLower
   where
   normalMkDumped "ast"    = Dump DumpAST
   normalMkDumped "parsed" = Dump DumpParsed
   normalMkDumped "arity"  = Dump DumpArity
   normalMkDumped other    = Dump DumpNothing

defaultMaxSteps :: Integer 
defaultMaxSteps = 1000

mkMaxSteps :: String -> Flag 
mkMaxSteps [] = MaxTraceSteps defaultMaxSteps 
mkMaxSteps n
   | all isDigit n = MaxTraceSteps $ read n
   | otherwise = MaxTraceSteps defaultMaxSteps

getMaxTraceSteps :: [Flag] -> Integer 
getMaxTraceSteps flags =
      probeFlagsFirst flags probe defaultMaxSteps
      where probe (MaxTraceSteps i) = Just i
            probe other = Nothing

getTraceDir :: [Flag] -> String
getTraceDir flags =
   probeFlagsFirst flags probe defaultTraceDir
      where probe (TraceDir d) = Just d
            probe other = Nothing

getEvalStyle :: [Flag] -> EvalStyle
getEvalStyle flags =
   probeFlagsFirst flags probe defaultEvalStyle
   where
   probe :: Flag -> Maybe EvalStyle
   probe (Style style) = Just style
   probe other = Nothing
