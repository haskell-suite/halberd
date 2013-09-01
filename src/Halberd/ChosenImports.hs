module Halberd.ChosenImports where

import Data.Map (Map)
import Data.Monoid
import Language.Haskell.Exts.Annotated
import Language.Haskell.Exts.Utils
import qualified Data.Map                as Map
import qualified Distribution.ModuleName as Cabal

data ChosenImports = ChosenImports
  { qualifieds   :: Map (ModuleName ()) Cabal.ModuleName
  , unqualifieds :: [Cabal.ModuleName]
  }

instance Monoid ChosenImports where
  mempty = ChosenImports
    { qualifieds   = mempty
    , unqualifieds = mempty
    }
  i1 `mappend` i2 = ChosenImports
    { qualifieds   = qualifieds   i1 `mappend`  qualifieds i2
    , unqualifieds = unqualifieds i1 `mappend`  unqualifieds i2
    }

lookupQualified :: ModuleName () -> ChosenImports -> Maybe Cabal.ModuleName
lookupQualified qualification = Map.lookup qualification . qualifieds

insertQualified :: ModuleName () -> Cabal.ModuleName -> ChosenImports -> ChosenImports
insertQualified qualification module_ chosenImports = chosenImports
  { qualifieds = Map.insert qualification module_ (qualifieds chosenImports) }

insertUnqualified :: Cabal.ModuleName -> ChosenImports -> ChosenImports
insertUnqualified module_ chosenImports = chosenImports
  { unqualifieds = module_ : unqualifieds chosenImports }

insertChoice :: QName a -> Cabal.ModuleName -> ChosenImports -> ChosenImports
insertChoice qname module_ =
  case getQualification qname of
    Just qualification -> insertQualified qualification module_
    Nothing            -> insertUnqualified module_
