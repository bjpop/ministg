-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.DumpState
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Representation of the state of the ministg evaluator.
-----------------------------------------------------------------------------

module Ministg.DumpState (dumpState) where

import Control.Monad.Trans (liftIO)
import Control.Monad.State (gets)
import Text.XHtml.Transitional as Html
import Text.XHtml.Table
import Data.Map as Map (toList)
import Ministg.AST
import Ministg.CallStack (CallStack, push, showCallStack)
import Ministg.Pretty as Pretty (pretty, Doc, ($$), nest, render, text)
import Ministg.State

dumpDir :: FilePath
dumpDir = "dump"

dumpState :: Exp -> Stack -> Heap -> Eval ()
dumpState exp stack heap = do
   dumpFile <- nextDumpFileName
   html <- makeHtml exp stack heap
   liftIO $ writeFile dumpFile $ renderHtml html 

nextDumpFileName :: Eval FilePath
nextDumpFileName = do
   count <- gets state_stepCount
   let filename = dumpDir ++ "/" ++ mkHtmlFileName count 
   return filename

makeHtml :: Exp -> Stack -> Heap -> Eval Html 
makeHtml exp stack heap = do
   count <- gets state_stepCount
   return (theHead count +++ theBody count)
   where
   theHead count = header << thetitle << ("Step " ++ show count)
   theBody count = 
      body << (mainHeading +++ navigation +++ expStackSection +++ heapSection )
      where
      mainHeading = h1 << ("Step " ++ show count)
      navigation = paragraph (previous +++ " " +++ next)
      expStackSection = (h3 << "Stack and Code") +++ expStackTable exp stack
      heapSection = (h3 << "Heap") +++ heapTable heap 
      previous = (anchor << "previous") ! [href $ mkHtmlFileName (count - 1)]
      next = (anchor << "next") ! [href $ mkHtmlFileName (count + 1)]

expStackTable :: Exp -> Stack -> Html
expStackTable exp stack
   = simpleTable [border 3, cellpadding 10] [] [headingRow,dataRow]
   where
   headingRow = [stringToHtml "Stack", stringToHtml "Expression"] 
   dataRow = [stackTable stack, pre << expStr]
   expStr = render $ pretty exp

stackTable :: Stack -> Html
stackTable [] = noHtml
stackTable stack
   = simpleTable [border 1, cellpadding 5, cellspacing 0] [] (map stackRow stack)
   where
   stackRow :: Continuation -> [Html]
   stackRow cont = [ pre << (render $ pretty cont) ]

heapTable :: Heap -> Html
heapTable heap
   = simpleTable [ border 3, cellpadding 5, cellspacing 0] [] (map heapRow mappings)
   where
   mappings = Map.toList heap
   heapRow :: (Var, Object) -> [Html]
   heapRow (var, obj) = [pre << var, pre << render (pretty obj)]

mkHtmlFileName :: Int -> FilePath
mkHtmlFileName count = "step" ++ show count ++ ".html"
