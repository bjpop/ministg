{-# OPTIONS_GHC -XPatternGuards #-}
-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.TraceEval
-- Copyright   : (c) 2009-2012 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : florbitous@gmail.com
-- Stability   : experimental
-- Portability : ghc
--
-- Trace the evaluation steps of the interpreter and generate HTML output. 
-----------------------------------------------------------------------------

module Ministg.TraceEval (traceEval, traceEnd) where

import System.FilePath ((<.>), (</>))
import Control.Monad (when, join)
import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets)
import Control.Applicative ((<$>))
import Text.XHtml.Transitional as Html
import Text.XHtml.Table hiding ((</>))
import Data.Map as Map (toList)
import Ministg.AST
import Ministg.CallStack (CallStack, push, showCallStack)
import Ministg.Pretty as Pretty (pretty, Doc, ($$), nest, render, text)
import Ministg.State
import Data.List as List (sortBy)

traceEval :: Exp -> Stack -> Heap -> Eval ()
traceEval exp stack heap = do
   traceOn <- gets state_trace
   when traceOn $ do 
      count <- gets state_stepCount
      maxSteps <- gets state_maxTraceSteps
      when (count <= maxSteps) $ do
         join (writeTraceFile <$> makeHtml exp stack heap)
      when (count == maxSteps + 1) $ lastTracePage "Maximum trace steps exceeded"

traceEnd :: Eval ()
traceEnd = do
   traceOn <- gets state_trace
   when traceOn $ lastTracePage "The computation has completed"
   
lastTracePage :: String -> Eval ()
lastTracePage msg = join (writeTraceFile <$> lastPage msg) 

writeTraceFile :: Html -> Eval ()
writeTraceFile html = do
   traceFile <- nextTraceFileName
   liftIO $ writeFile traceFile $ renderHtml html 

nextTraceFileName :: Eval FilePath
nextTraceFileName = do
   traceDir <- gets state_traceDir
   count <- gets state_stepCount
   return $ traceDir </> mkHtmlFileName count 

lastPage :: String -> Eval Html
lastPage msg = do
   count <- gets state_stepCount
   return (theHead +++ theBody count)
   where
   theHead = header << thetitle << msg 
   theBody count 
      = body << (mainHeading +++ navigation)
      where
      mainHeading = h1 << msg 
      navigation = paragraph ((anchor << "previous") ! [href $ mkHtmlFileName (count - 1)])

makeHtml :: Exp -> Stack -> Heap -> Eval Html 
makeHtml exp stack heap = do
   count <- gets state_stepCount
   rule <- gets state_lastRule
   callStack <- gets state_callStack
   wantCallStack <- gets state_traceCallStack
   return $ headAndBody count rule 
               (if wantCallStack then Just callStack else Nothing) 
   where
   headAndBody count rule maybeCallStack = theHead +++ theBody
      where
      stepStr = "Step " ++ show count
      theHead = header << thetitle << stepStr 
      theBody = 
         body << (mainHeading +++ navigation +++ ruleSection +++ expStackSection +++ heapSection)
         where
         mainHeading = h1 << stepStr 
         navigation = paragraph (previous +++ " " +++ next)
         previous = if count == 0 then noHtml 
                       else (anchor << "previous") ! [href $ mkHtmlFileName (count - 1)]
         next = (anchor << "next") ! [href $ mkHtmlFileName (count + 1)]
         ruleSection = if null rule then noHtml 
                          else (h3 << "Most recent rule applied") +++ (paragraph << rule)
         expStackSection = (h3 << "Stack and Code") +++ expStackCallTable exp stack maybeCallStack 
         heapSection = (h3 << "Heap") +++ heapTable heap 

expStackCallTable :: Exp -> Stack -> Maybe CallStack -> Html
expStackCallTable exp stack maybeCallStack 
   = simpleTable [border 3, cellpadding 10] [thestyle "vertical-align:top"] rows 
   where
   rows | Just callStack <- maybeCallStack =
             [stackExprHeading ++ callStackHeading, stackExprData ++ callStackData callStack]
        | otherwise = [stackExprHeading, stackExprData] 
        where
        stackExprHeading = [stringToHtml "Stack", stringToHtml "Expression"]
        stackExprData = [stackTable stack, pre << expStr]
        callStackHeading = [stringToHtml "Call Stack"]
        callStackData callStack = [callStackTable callStack]
        expStr = render $ pretty exp

stackTable :: Stack -> Html
stackTable [] = noHtml
stackTable stack
   = simpleTable [border 1, cellpadding 5, cellspacing 0] 
                 [] (map stackRow stack)
   where
   stackRow :: Continuation -> [Html]
   stackRow cont = [ pre << (render $ pretty cont) ]

callStackTable :: CallStack -> Html
callStackTable [] = noHtml
callStackTable stack
   = simpleTable [border 1, cellpadding 5, cellspacing 0] 
                 [] (map stackRow stack)
   where
   stackRow :: String -> [Html]
   stackRow str = [ pre << str ]

heapTable :: Heap -> Html
heapTable heap
   = simpleTable [border 3, cellpadding 5, cellspacing 0] 
                 [] (headingRow : map heapRow mappings)
   where
   headingRow = [stringToHtml "Variable", stringToHtml "Object"]
   mappings = List.sortBy (\(x,_) (y,_) -> compare x y) $ Map.toList heap
   heapRow :: (Var, Object) -> [Html]
   heapRow (var, obj) = [pre << var, pre << render (pretty obj)]

mkHtmlFileName :: Integer -> FilePath
mkHtmlFileName count = "step" ++ show count <.> "html"
