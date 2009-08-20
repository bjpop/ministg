-----------------------------------------------------------------------------
-- |
-- Module      : Ministg.Pretty
-- Copyright   : (c) 2009 Bernie Pope 
-- License     : BSD-style
-- Maintainer  : bjpop@csse.unimelb.edu.au
-- Stability   : experimental
-- Portability : ghc
--
-- Convenient class wrapper for pretty printing. 
-----------------------------------------------------------------------------

module Ministg.Pretty (module Ministg.Pretty, module HPJ) where

import Text.PrettyPrint.HughesPJ as HPJ

class Pretty a where
   pretty :: a -> Doc

prettyText :: Pretty a => a -> String
prettyText = render . pretty

parensIf :: Pretty a => (a -> Bool) -> a -> Doc
parensIf test x = if test x then parens $ pretty x else pretty x 

tuple :: Pretty a => [a] -> Doc
tuple = parens . hsep . punctuate comma . map pretty 

instance Pretty Int where
  pretty = int

instance Pretty Integer where
  pretty = integer

instance Pretty Char where
  pretty = char

instance Pretty Double where
   pretty = double

instance Pretty Bool where
  pretty True = text "True"
  pretty False = text "False"

instance Pretty a => Pretty (Maybe a) where
   pretty Nothing = empty
   pretty (Just x) = pretty x
