module Halberd.ChosenImports where

import Control.Monad
import Data.List
import Data.Map (Map, insertWith)
import Data.Monoid
import Language.Haskell.Exts.Annotated hiding (name)
import qualified Data.Map                as Map
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Text       as Cabal

data ChosenImports = ChosenImports
  { qualifieds   :: Map (ModuleName ()) Cabal.ModuleName
  , unqualifieds :: Map Cabal.ModuleName [Name ()]
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

insertUnqualified :: Cabal.ModuleName -> Name () -> ChosenImports -> ChosenImports
insertUnqualified module_ name chosenImports = chosenImports
  { unqualifieds = insertWith (++) module_ [name] (unqualifieds chosenImports) }

insertChoice :: QName a -> Cabal.ModuleName -> ChosenImports -> ChosenImports
insertChoice qname module_ =
  case qname of
    Qual _ qualification _ -> insertQualified (void qualification) module_
    UnQual _ name          -> insertUnqualified module_ (void name)
    Special _ _            -> error "impossible: insertChoice"

isEmpty :: ChosenImports -> Bool
isEmpty ci = Map.null (qualifieds ci) && Map.null (unqualifieds ci)

showChosenImports :: ChosenImports -> [String]
showChosenImports ci = showQualifieds (qualifieds ci) ++ showUnqualifieds (unqualifieds ci)

showQualifieds :: Map (ModuleName ()) Cabal.ModuleName -> [String]
showQualifieds = map (uncurry showQualified) . Map.toList

showUnqualifieds :: Map Cabal.ModuleName [Name ()] -> [String]
showUnqualifieds = map (uncurry showUnqualified) . Map.toList

showQualified :: ModuleName () -> Cabal.ModuleName -> String
showQualified qualification modName =
    intercalate " "
      [ "import"
      , "qualified"
      , Cabal.display modName
      , "as"
      , prettyPrint qualification
      ]
showUnqualified :: Cabal.ModuleName -> [Name ()] -> String
showUnqualified modName names =
    intercalate " "
      [ "import"
      , Cabal.display modName
      , "("
      ,  intercalate ", " $ map prettyPrint names
      , ")"
      ]
