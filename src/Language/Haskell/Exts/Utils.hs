module Language.Haskell.Exts.Utils where

import Control.Monad
import Language.Haskell.Exts.Annotated

getQualification :: QName a -> Maybe (ModuleName ())
getQualification (Qual _ q _) = Just $ void q
getQualification _            = Nothing
