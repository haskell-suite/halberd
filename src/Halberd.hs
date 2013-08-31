{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE DoAndIfThenElse      #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (forM_)
import           Control.Monad.State hiding (forM_)
import           Data.Foldable (forM_)
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
         (allValueSuggestions, allTypeSuggestions) <- evalModuleT (suggestedImports module_) pkgs suffix readInterface

         let (valueSuggestions, noValueSuggestions) = partition (not . null . snd) allValueSuggestions
         let (typeSuggestions,  noTypeSuggestions)  = partition (not . null . snd) allTypeSuggestions
         let noSuggestions = map fst noValueSuggestions ++ map fst noTypeSuggestions

         (valueChoices, typeChoices) <- flip evalStateT mempty $ do
           valueChoices <- askUserChoices valueSuggestions
           typeChoices <- askUserChoices typeSuggestions
           return (valueChoices, typeChoices)
         let allImports = map (uncurry toImport) valueChoices ++ map (uncurry toImport) typeChoices
         let imports = mergeExplicitImports allImports

         when (not . null $ noSuggestions) $ do
           putStrLn "------------- Could not find import for -------------"
           forM_ noSuggestions $ \q -> do
             putStrLn $ " - " ++ prettyPrint q
           putStrLn ""

         when (not . null $ imports) $ do
           putStrLn "-------- Insert these imports into your file --------"
           putStrLn ""
           putStrLn $ unlines (map showImport imports)
  where
    suffix = "names"

type Suggestion a = (QName (Scoped SrcSpan), [CanonicalSymbol a])
type Choice a = (QName (Scoped SrcSpan), CanonicalSymbol a)
type ChosenImports = Map (ModuleName ()) Cabal.ModuleName

data Import = Qualified (ModuleName ()) Cabal.ModuleName
            | Explicit Cabal.ModuleName [Name ()]

moduleName :: Import -> Cabal.ModuleName
moduleName (Qualified _ n) = n
moduleName (Explicit n _)  = n

isExplicit :: Import -> Bool
isExplicit Explicit{} = True
isExplicit _          = False

mergeExplicitImports :: [Import] -> [Import]
mergeExplicitImports ims = foldr merge [] $ sortBy (comparing $ isExplicit &&& moduleName) ims
  where
    merge (Explicit m1 nms1) (Explicit m2 nms2 : is) | m1 == m2 = Explicit m1 (nms1 ++ nms2) : is
    merge i                 is                                  = i : is

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

askUserChoices :: [Suggestion a] -> StateT ChosenImports IO [Choice a]
askUserChoices suggestions = fmap catMaybes . forM suggestions $ \(qname, modules) ->
  do chosenModules <- get
     if alreadyChosen qname modules chosenModules
     then
       return Nothing
     else do
       choice <- askUserChoice qname modules
       forM_ (getQualification qname) $ \qualification -> modify $ Map.insert qualification (snd3 choice)
       return $ Just (qname, choice)
  where
    alreadyChosen qname modules chosenModules = fromMaybe False $
      do q <- getQualification qname
         module_ <- Map.lookup q chosenModules
         return $ module_ `elem` map snd3 modules
    snd3 (_, y, _) = y
    getQualification (Qual _ q _) = Just $ void q
    getQualification _            = Nothing

askUserChoice :: MonadIO m => QName (Scoped SrcSpan) -> [CanonicalSymbol a] -> m (CanonicalSymbol a)
askUserChoice _     [suggestion] = return suggestion
askUserChoice qname suggestions  = liftIO $
  do putStrLn $ prettyPrint qname ++ ":"
     forM_ (zip [1 :: Integer ..] suggestions) $ \(i, (_, modName, _)) -> putStrLn $ show i ++ ") " ++ Cabal.display modName
     putStrLn ""
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


toImport :: QName a -> CanonicalSymbol b -> Import
toImport qname (_, modName, _) =
  case qname of
    Qual _ qualification _ -> Qualified (void qualification) modName
    UnQual _ nm            -> Explicit modName [void nm]
    Special _ _            -> error "impossible: toImport"


showImport :: Import -> String
showImport (Qualified qualification modName) =
    intercalate " "
      [ "import"
      , "qualified"
      , Cabal.display modName
      , "as"
      , prettyPrint qualification
      ]
showImport (Explicit modName names) =
    intercalate " "
      [ "import"
      , Cabal.display modName
      , "("
      ,  intercalate ", " $ map prettyPrint names
      , ")"
      ]

toLookupTable :: Ord k => (a -> k) -> Set a -> Map k [a]
toLookupTable key = Map.fromList
                  . map (fst . head &&& map snd)
                  . groupBy ((==) `on` fst)
                  . sortBy (comparing fst)
                  . map (key &&& id)
                  . Set.toList

