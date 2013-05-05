{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections        #-}

import           Debug.Trace

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.Generics
import           Data.List
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Ord
import           Data.Proxy
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Distribution.HaskellSuite.Helpers
import           Distribution.HaskellSuite.Tool
import qualified Distribution.InstalledPackageInfo   as Cabal
import qualified Distribution.ModuleName             as Cabal
import qualified Distribution.Package                as Cabal
import           Distribution.Simple.Compiler
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Modules
import           Language.Haskell.Modules.Imports    ()
import           Language.Haskell.Modules.Interfaces
import           System.FilePath
import           Text.Show.Pretty

import           Halberd.CollectNames                (collectUnboundNames)

main =
  do (ParseOk module_) <- parseFile "test.hs"
     pkgs <- concat <$> mapM (toolGetInstalledPkgs theTool) [UserPackageDB, GlobalPackageDB]
     bla <- evalModuleT (suggestedImports module_) pkgs retrieveModuleInfo Map.empty
     putStrLn (ppShow bla)

type CanonicalSymbol a = (PackageRef, Cabal.ModuleName, a OrigName)

data PackageRef = PackageRef
  { installedPackageId :: Cabal.InstalledPackageId
  , sourcePackageId    :: Cabal.PackageId
  } deriving (Eq, Ord, Show)

toPackageRef :: Cabal.InstalledPackageInfo_ m -> PackageRef
toPackageRef pi = PackageRef (Cabal.installedPackageId pi) (Cabal.sourcePackageId pi)

suggestedImports module_ =
  do pkgs <- getPackages
     [(annSrc, _)] <- analyseModules [fmap srcInfoSpan module_]
     let x@(typeNames, valueNames) = collectUnboundNames annSrc
     trace (show x) $ do
     (valueDefs, typeDefs) <-
       fmap mconcat $ forM pkgs $ \pkg ->
         fmap mconcat $ forM (Cabal.exposedModules pkg) $ \exposedModule -> do
            (Symbols values types) <- readModuleInfo (Cabal.libraryDirs pkg) exposedModule
            return (Set.map (toPackageRef pkg, exposedModule,) values, Set.map (toPackageRef pkg, exposedModule,) types)
     let valueTable = toLookupTable (gUnqual . sv_origName . trd) valueDefs
         typeTable  = toLookupTable (gUnqual . st_origName . trd) typeDefs
         names      = mapMaybe unQName valueNames
     return (map (flip Map.lookup valueTable) names)

gUnqual (GName _ name) = name

trd (_, _, z) = z

unQName (Qual    _ _ name) = Just (strName name)
unQName (UnQual  _   name) = Just (strName name)
unQName (Special _ _     ) = Nothing

strName (Ident  _ str) = str
strName (Symbol _ str) = str


toLookupTable :: Ord k => (a -> k) -> Set a -> Map k [a]
toLookupTable key = Map.fromList
                  . map (fst . head &&& map snd)
                  . groupBy ((==) `on` fst)
                  . sortBy (comparing fst)
                  . map (key &&& id)
                  . Set.toList

{-
     Suggestion = (Packages, Module, Name)

     [(Name, Suggestion)]

     Map Name [Suggestion]

     Map UnboundName [Suggestion]
     -}


-- This function says how we actually find and read the module
-- information, given the search path and the module name
retrieveModuleInfo :: [FilePath] -> Cabal.ModuleName -> IO Symbols
retrieveModuleInfo dirs name = do
  (base, rel) <- findModuleFile dirs [suffix] name
  readInterface $ base </> rel

theTool :: SimpleTool
theTool =
  simpleTool
    "haskell-modules"
    undefined
    knownExtensions
    (return Nothing)
    undefined
    [suffix]

suffix = "names"

