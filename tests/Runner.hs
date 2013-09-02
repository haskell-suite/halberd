{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances, OverloadedStrings #-}
import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Monoid
import Data.List.Split
import Data.Proxy
import Data.String
import Distribution.HaskellSuite
import Distribution.Simple.Compiler
import Language.Haskell.Exts.Annotated hiding (fileName)
import Language.Haskell.Names.Interfaces
import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (testCase)
import Test.HUnit (assertEqual, Assertion)
import qualified Data.Map                as Map
import qualified Distribution.ModuleName as Cabal

import Halberd.ChosenImports (ChosenImports (..))
import Halberd.Suggestions

main :: IO ()
main = do
  pkgs <- concat <$> mapM (getInstalledPackages (Proxy :: Proxy NamesDB)) [UserPackageDB, GlobalPackageDB]
  defaultMain [ testCase "Collect unbound" (collectUnbound pkgs)
              , testCase "Resolve single option" (resolveSingleOption pkgs)
              , testCase "Resolve multiple options" (resolveMultiple pkgs)
              , testCase "Resolve multiple options (different order)" (resolveMultipleDifferentOrder pkgs)
              ]

collectUnbound :: Packages -> Assertion
collectUnbound pkgs = do
  (ParseOk module_) <- parseFile "tests/input/Unbound.hs"
  (unboundTypes, unboundValues) <- evalModuleT (findUnbound module_) pkgs "names" readInterface
  assertEqual "Unbound types"  (void <$> unboundTypes)  ["M.Map", "Int8"]
  assertEqual "Unbound values" (void <$> unboundValues) ["forM", "M.fromList"]

testResolution :: FilePath -> ChosenImports -> Packages -> Assertion
testResolution fileName expectedImports pkgs = do
  (ParseOk module_) <- parseFile fileName
  suggestions <- evalModuleT (suggestedImports module_) pkgs "names" readInterface
  let (newSuggestions, imports) = runState (resolveSuggestions suggestions) mempty
  assertEqual "resolved suggestions" [] newSuggestions
  assertEqual "chosen imports" expectedImports imports

resolveSingleOption :: Packages -> Assertion
resolveSingleOption = testResolution "tests/input/ResolveSingle.hs"
    ChosenImports { qualifieds   = Map.fromList []
                  , unqualifieds = Map.fromList [("Control.Applicative", ["pure"])]
                  }

resolveMultiple :: Packages -> Assertion
resolveMultiple = testResolution "tests/input/ResolveMultiple.hs"
    ChosenImports { qualifieds   = Map.fromList []
                  , unqualifieds = Map.fromList [("Control.Applicative", ["<$>", "pure"])]
                  }

resolveMultipleDifferentOrder :: Packages -> Assertion
resolveMultipleDifferentOrder = testResolution "tests/input/ResolveMultipleDifferentOrder.hs"
    ChosenImports { qualifieds   = Map.fromList []
                  , unqualifieds = Map.fromList [("Control.Applicative", ["<$>", "pure"])]
                  }

-- Some instances to make it easier to write down expected values in
-- assertions.

instance IsString (QName ()) where
  fromString str =
    let parts = splitOn "." str
    in  case parts of
          [nm]      -> UnQual () (fromString nm)
          [qual,nm] -> Qual () (fromString qual) (fromString nm)
          _         -> error "Too many dots in IsString Name."

instance IsString (ModuleName ()) where
  fromString = ModuleName ()

instance IsString (Name ()) where
  fromString str =
    if (isAlpha (head str) || head str == '_')
    then Ident  () str
    else Symbol () str

instance IsString Cabal.ModuleName where
  fromString = Cabal.fromString
