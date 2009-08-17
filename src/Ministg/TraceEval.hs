-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.TraceEval
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Trace the evaluation steps of the interpreter and generate HTML output. 
-----------------------------------------------------------------------------

module Ministg.TraceEval (traceEval) where

import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets)
import Text.XHtml.Transitional as Html
import Text.XHtml.Table
import Data.Map as Map (toList)
import Ministg.AST
import Ministg.CallStack (CallStack, push, showCallStack)
import Ministg.Pretty as Pretty (pretty, Doc, ($$), nest, render, text)
import Ministg.State
import Data.List as List (sortBy)

traceDir :: FilePath
traceDir = "trace"

traceEval :: Exp -> Stack -> Heap -> Eval ()
traceEval exp stack heap = do
   traceFile <- nextTraceFileName
   html <- makeHtml exp stack heap
   liftIO $ writeFile traceFile $ renderHtml html 

nextTraceFileName :: Eval FilePath
nextTraceFileName = do
   count <- gets state_stepCount
   return $ traceDir ++ "/" ++ mkHtmlFileName count 

makeHtml :: Exp -> Stack -> Heap -> Eval Html 
makeHtml exp stack heap = do
   count <- gets state_stepCount
   rule <- gets state_lastRule
   return $ headAndBody count rule
   where
   headAndBody count rule = theHead +++ theBody
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
         expStackSection = (h3 << "Stack and Code") +++ expStackTable exp stack
         heapSection = (h3 << "Heap") +++ heapTable heap 

expStackTable :: Exp -> Stack -> Html
expStackTable exp stack
   = simpleTable [border 3, cellpadding 10] [thestyle "vertical-align:top"] [headingRow,dataRow]
   where
   headingRow = [stringToHtml "Stack", stringToHtml "Expression"] 
   dataRow = [stackTable stack, pre << expStr]
   expStr = render $ pretty exp

stackTable :: Stack -> Html
stackTable [] = noHtml
stackTable stack
   = simpleTable [border 1, cellpadding 5, cellspacing 0] 
                 [] (map stackRow stack)
   where
   stackRow :: Continuation -> [Html]
   stackRow cont = [ pre << (render $ pretty cont) ]

heapTable :: Heap -> Html
heapTable heap
   = simpleTable [border 3, cellpadding 5, cellspacing 0] 
                 [] (headingRow : map heapRow mappings)
   where
   headingRow = [stringToHtml "Variable", stringToHtml "Object"]
   mappings = List.sortBy (\(x,_) (y,_) -> compare x y) $ Map.toList heap
   heapRow :: (Var, Object) -> [Html]
   heapRow (var, obj) = [pre << var, pre << render (pretty obj)]

mkHtmlFileName :: Int -> FilePath
mkHtmlFileName count = "step" ++ show count ++ ".html"
