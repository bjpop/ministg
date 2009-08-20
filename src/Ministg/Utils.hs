-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.Utils
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Some handy utilities. 
-----------------------------------------------------------------------------
module Ministg.Utils where

import Control.Monad 
   ( liftM )

safeReadFile :: FilePath -> IO (Either String String) 
safeReadFile file
   = catch (rightReadFile file) $ \error -> return $ Left $ show error 
   where
   rightReadFile :: FilePath -> IO (Either String String)
   rightReadFile file = liftM Right $ readFile file
