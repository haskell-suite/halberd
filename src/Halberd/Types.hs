module Halberd.Types where

import qualified Distribution.InstalledPackageInfo   as Cabal
import qualified Distribution.ModuleName             as Cabal
import qualified Distribution.Package                as Cabal
import           Language.Haskell.Names

type CanonicalSymbol = (PackageRef, Cabal.ModuleName, OrigName)

data PackageRef = PackageRef
  { installedPackageId :: Cabal.InstalledPackageId
  , sourcePackageId    :: Cabal.PackageId
  } deriving (Eq, Ord, Show)

toPackageRef :: Cabal.InstalledPackageInfo_ m -> PackageRef
toPackageRef pkgInfo =
    PackageRef { installedPackageId = Cabal.installedPackageId pkgInfo
               , sourcePackageId    = Cabal.sourcePackageId    pkgInfo
               }
