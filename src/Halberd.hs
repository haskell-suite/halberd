{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections        #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Monoid
import           Data.Ord
import           Data.Proxy
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Distribution.HaskellSuite.Helpers
import           Distribution.HaskellSuite.PackageDB
import qualified Distribution.InstalledPackageInfo   as Cabal
import qualified Distribution.ModuleName             as Cabal
import qualified Distribution.Package                as Cabal
import           Distribution.Simple.Compiler
import qualified Distribution.Text                   as Cabal
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Modules
import           Language.Haskell.Modules.Imports    ()
import           Language.Haskell.Modules.Interfaces
import           System.Environment
import           System.Exit
import           System.FilePath

import           Halberd.CollectNames                (collectUnboundNames)

main :: IO ()
main =
  do args <- getArgs
     case args of
       [] -> do
         putStrLn "Usage: halberd <SOURCEFILE>"
         exitFailure
       (fileName:_) -> do
         (ParseOk module_) <- parseFile "test.hs"
         pkgs <- concat <$>
           mapM
             (getInstalledPackages Don'tInitDB (Proxy :: Proxy NamesDB))
             [UserPackageDB, GlobalPackageDB]
         bla <- evalModuleT (suggestedImports module_) pkgs retrieveModuleInfo Map.empty
         putStrLn bla

type CanonicalSymbol a = (PackageRef, Cabal.ModuleName, a OrigName)

data PackageRef = PackageRef
  { installedPackageId :: Cabal.InstalledPackageId
  , sourcePackageId    :: Cabal.PackageId
  } deriving (Eq, Ord, Show)

toPackageRef :: Cabal.InstalledPackageInfo_ m -> PackageRef
toPackageRef pkgInfo =
    PackageRef (Cabal.installedPackageId pkgInfo)
               (Cabal.sourcePackageId pkgInfo)

suggestedImports :: Module SrcSpanInfo -> ModuleT Symbols IO String
suggestedImports module_ =
  do pkgs <- getPackages
     [(annSrc, _)] <- analyseModules [fmap srcInfoSpan module_]
     let (typeNames, valueNames) = collectUnboundNames annSrc
     (valueDefs, typeDefs) <-
       fmap mconcat $ forM pkgs $ \pkg ->
         fmap mconcat $ forM (Cabal.exposedModules pkg) $ \exposedModule -> do
            (Symbols values types) <- readModuleInfo (Cabal.libraryDirs pkg) exposedModule
            return (Set.map (toPackageRef pkg, exposedModule,) values, Set.map (toPackageRef pkg, exposedModule,) types)
     let valueTable = toLookupTable (gUnqual . sv_origName . trd) valueDefs
         typeTable  = toLookupTable (gUnqual . st_origName . trd) typeDefs
     return $ (unlines $ nub $ map (toImportStatements "value" valueTable) valueNames)
              ++ (unlines $ nub $ map (toImportStatements "type" typeTable) typeNames)
  where
    trd (_, _, z)        = z
    gUnqual (GName _ n)  = n




toImportStatements :: String
                   -> Map String [(CanonicalSymbol a)]
                   -> QName (Scoped SrcSpan)
                   -> String
toImportStatements nameSpace symbolTable qname = unlines $ case definitions of
    Nothing   -> ["-- Could not find " ++ nameSpace ++ ": " ++ prettyPrint qname]
    Just defs -> map mkImport defs
  where
    definitions = do
        n <- unQName qname
        Map.lookup n symbolTable

    mkImport (_, moduleName, _) = case qname of
        Qual _ qualification _ -> intercalate " "
          [ "import"
          , "qualified"
          , Cabal.display moduleName
          , "as"
          , prettyPrint qualification
          ]
        UnQual _ n -> intercalate " "
          [ "import"
          , Cabal.display moduleName
          , "("
          , prettyPrint n
          , ")"
          ]
        Special _ _ -> error "impossible: toImportStatements"

    unQName (Qual    _ _ n) = Just (strName n)
    unQName (UnQual  _   n) = Just (strName n)
    unQName (Special _ _  ) = Nothing

    strName (Ident  _ str)  = str
    strName (Symbol _ str)  = str


toLookupTable :: Ord k => (a -> k) -> Set a -> Map k [a]
toLookupTable key = Map.fromList
                  . map (fst . head &&& map snd)
                  . groupBy ((==) `on` fst)
                  . sortBy (comparing fst)
                  . map (key &&& id)
                  . Set.toList


-- This function says how we actually find and read the module
-- information, given the search path and the module name
retrieveModuleInfo :: [FilePath] -> Cabal.ModuleName -> IO Symbols
retrieveModuleInfo dirs n = do
    (base, rel) <- findModuleFile dirs [suffix] n
    readInterface $ base </> rel
  where
    suffix = "names"
