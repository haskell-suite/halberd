{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections        #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Monoid
import           Data.Ord
import           Data.Proxy
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Distribution.HaskellSuite
import qualified Distribution.InstalledPackageInfo   as Cabal
import qualified Distribution.ModuleName             as Cabal
import qualified Distribution.Package                as Cabal
import           Distribution.Simple.Compiler
import qualified Distribution.Text                   as Cabal
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names
import           Language.Haskell.Names.Imports      ()
import           Language.Haskell.Names.Interfaces
import           Safe
import           System.Environment
import           System.Exit
import           System.IO

import           Halberd.CollectNames                (collectUnboundNames)

main :: IO ()
main =
  do args <- getArgs
     case args of
       [] -> do
         putStrLn "Usage: halberd <SOURCEFILE>"
         exitFailure
       (file:_) -> do
         (ParseOk module_) <- parseFile file
         pkgs <- concat <$>
           mapM
             (getInstalledPackages (Proxy :: Proxy NamesDB))
             [UserPackageDB, GlobalPackageDB]
         (valueSuggestions, typeSuggestions) <- evalModuleT (suggestedImports module_) pkgs suffix readInterface
         valueChoices <- askUserChoices $ filter (not . null . snd) valueSuggestions
         typeChoices <- askUserChoices $ filter (not . null . snd) typeSuggestions
         let imports = map (uncurry mkImport) valueChoices ++ map (uncurry mkImport) typeChoices
         putStrLn $ unlines imports
  where
    suffix = "names"

type Suggestion a = (QName (Scoped SrcSpan), [CanonicalSymbol a])

suggestedImports :: Module SrcSpanInfo -> ModuleT Symbols IO ([Suggestion SymValueInfo], [Suggestion SymTypeInfo])
suggestedImports module_ =
  do (unboundTypes, unboundValues) <- uniques <$> findUnbound module_
     (valueTable, typeTable) <- mkLookupTables
     let valueSuggestions = map (id &&& lookupDefinitions valueTable) unboundValues
         typeSuggestions  = map (id &&& lookupDefinitions typeTable ) unboundTypes
     return (valueSuggestions, typeSuggestions)
  where
    uniques = unique *** unique
    unique = nubBy ((==) `on` void)

askUserChoices :: [(QName (Scoped SrcSpan), [CanonicalSymbol a])] -> IO [(QName (Scoped SrcSpan), CanonicalSymbol a)]
askUserChoices suggestions = forM suggestions $ \(qname, modules) ->
  do choice <- askUserChoice qname modules
     return (qname, choice)

askUserChoice :: QName (Scoped SrcSpan) -> [CanonicalSymbol a] -> IO (CanonicalSymbol a)
askUserChoice qname suggestions =
  do putStrLn $ prettyPrint qname ++ ":"
     forM_ (zip [1 :: Integer ..] suggestions) $ \(i, (_, moduleName, _)) -> putStrLn $ show i ++ ") " ++ Cabal.display moduleName
     getChoice suggestions

getChoice :: [a] -> IO a
getChoice xs = withoutOutput go
  where
    go =
      do c <- getChar
         let mi = readMay [c]
         case (subtract 1) <$> mi >>= atMay xs of
           Nothing -> go
           Just x  -> return x
    withoutOutput action =
      do buffering <- hGetBuffering stdin
         echo <- hGetEcho stdout
         hSetBuffering stdin NoBuffering
         hSetEcho stdout False
         result <- action
         hSetBuffering stdin buffering
         hSetEcho stdout echo
         return result

type CanonicalSymbol a = (PackageRef, Cabal.ModuleName, a OrigName)

data PackageRef = PackageRef
  { installedPackageId :: Cabal.InstalledPackageId
  , sourcePackageId    :: Cabal.PackageId
  } deriving (Eq, Ord, Show)

toPackageRef :: Cabal.InstalledPackageInfo_ m -> PackageRef
toPackageRef pkgInfo =
    PackageRef { installedPackageId = Cabal.installedPackageId pkgInfo
               , sourcePackageId    = Cabal.sourcePackageId    pkgInfo
               }

findUnbound :: Module SrcSpanInfo -> ModuleT Symbols IO ([QName (Scoped SrcSpan)], [QName (Scoped SrcSpan)])
findUnbound module_ = collectUnboundNames <$> annotateModule Haskell98 [] (fmap srcInfoSpan module_)

type LookupTable a = Map String [CanonicalSymbol a]

mkLookupTables :: ModuleT Symbols IO (LookupTable SymValueInfo, LookupTable SymTypeInfo)
mkLookupTables =
  do pkgs <- getPackages
     (valueDefs, typeDefs) <-
       fmap mconcat $ forM pkgs $ \pkg ->
         fmap mconcat $ forM (Cabal.exposedModules pkg) $ \exposedModule -> do
            (Symbols values types) <- readModuleInfo (Cabal.libraryDirs pkg) exposedModule
            return (Set.map (toPackageRef pkg, exposedModule,) values, Set.map (toPackageRef pkg, exposedModule,) types)
     let valueTable = toLookupTable (gUnqual . sv_origName . trd) valueDefs
         typeTable  = toLookupTable (gUnqual . st_origName . trd) typeDefs
     return (valueTable, typeTable)
  where
    trd (_, _, z)        = z
    gUnqual (OrigName _ (GName _ n))  = n


lookupDefinitions :: Map String [CanonicalSymbol a] -> QName (Scoped SrcSpan) -> [CanonicalSymbol a]
lookupDefinitions symbolTable qname = fromMaybe [] $
  do n <- unQName qname
     Map.lookup n symbolTable
  where
    unQName (Qual    _ _ n) = Just (strName n)
    unQName (UnQual  _   n) = Just (strName n)
    unQName (Special _ _  ) = Nothing

    strName (Ident  _ str)  = str
    strName (Symbol _ str)  = str


mkImport :: QName a -> CanonicalSymbol b -> String
mkImport qname (_, moduleName, _) =
  case qname of
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

toLookupTable :: Ord k => (a -> k) -> Set a -> Map k [a]
toLookupTable key = Map.fromList
                  . map (fst . head &&& map snd)
                  . groupBy ((==) `on` fst)
                  . sortBy (comparing fst)
                  . map (key &&& id)
                  . Set.toList

