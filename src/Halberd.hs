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

import           Halberd.ChosenImports
import           Halberd.CollectNames                (collectUnboundNames)
import           Halberd.Import
import           Language.Haskell.Exts.Utils

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
         allSuggestions <- evalModuleT (suggestedImports module_) pkgs suffix readInterface

         let (suggestions, noSuggestions) = partition (not . null . snd) allSuggestions

         choices <- askUserChoices suggestions

         let allImports = map (uncurry toImport . second snd3) choices
         let imports = mergeExplicitImports allImports

         when (not . null $ noSuggestions) $ do
           putStrLn "------------- Could not find import for -------------"
           forM_ noSuggestions $ \(q, _) -> do
             putStrLn $ " - " ++ prettyPrint q
           putStrLn ""

         when (not . null $ imports) $ do
           putStrLn "-------- Insert these imports into your file --------"
           putStrLn ""
           putStrLn $ unlines (map showImport imports)
  where
    suffix = "names"

type Suggestion = (QName (Scoped SrcSpan), [CanonicalSymbol])
type Choice = (QName (Scoped SrcSpan), CanonicalSymbol)

suggestedImports :: Module SrcSpanInfo -> ModuleT Symbols IO [Suggestion]
suggestedImports module_ =
  do (unboundTypes, unboundValues) <- uniques <$> findUnbound module_
     (valueTable, typeTable) <- mkLookupTables
     let valueSuggestions = map (id &&& lookupDefinitions valueTable) unboundValues
         typeSuggestions  = map (id &&& lookupDefinitions typeTable ) unboundTypes
     return $ valueSuggestions ++ typeSuggestions
  where
    uniques = unique *** unique
    unique = nubBy ((==) `on` void)

askUserChoices :: [Suggestion] -> IO [Choice]
askUserChoices suggestions = snd <$> execStateT (go suggestions) mempty
  where
    go sugs = do
      remaining <- resolveSuggestions sugs
      case remaining of
        [] -> return []
        ((qname, modules):ss) -> do
          choice <- askUserChoice qname modules
          modify $ insertChoice qname (snd3 choice) *** ((qname, choice):)
          go ss

resolveSuggestions :: [Suggestion] -> StateT (ChosenImports, [Choice]) IO [Suggestion]
resolveSuggestions suggestions = fmap catMaybes . forM suggestions $ \suggestion@(qname, modules) ->
  do chosenModules <- gets fst
     if alreadyChosen qname modules chosenModules
     then
       return Nothing
     else do
       case hasSingleOption qname modules chosenModules of
         Nothing           -> return $ Just suggestion
         Just choice -> do
           modify $ insertChoice qname (snd3 choice) *** ((qname, choice):)
           return Nothing
  where
    alreadyChosen qname modules chosenModules = fromMaybe False $
      do q <- getQualification qname
         module_ <- lookupQualified q chosenModules
         return $ module_ `elem` map snd3 modules
    hasSingleOption _        [module_] _             = Just module_
    hasSingleOption UnQual{} modules   chosenModules | singleOrigName modules =
      headMay $ filter ((`elem` unqualifieds chosenModules) . snd3) modules
    hasSingleOption _        _         _             = Nothing
    singleOrigName = allEqual . map trd3
    allEqual []     = True
    allEqual (x:xs) = all (== x) xs

askUserChoice :: MonadIO m => QName (Scoped SrcSpan) -> [CanonicalSymbol] -> m CanonicalSymbol
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

findUnbound :: Module SrcSpanInfo -> ModuleT Symbols IO ([QName (Scoped SrcSpan)], [QName (Scoped SrcSpan)])
findUnbound module_ = collectUnboundNames <$> annotateModule Haskell98 [] (fmap srcInfoSpan module_)

type LookupTable = Map String [CanonicalSymbol]

mkLookupTables :: ModuleT Symbols IO (LookupTable, LookupTable)
mkLookupTables =
  do pkgs <- getPackages
     (valueDefs, typeDefs) <-
       fmap mconcat $ forM pkgs $ \pkg ->
         fmap mconcat $ forM (Cabal.exposedModules pkg) $ \exposedModule -> do
            (Symbols values types) <- readModuleInfo (Cabal.libraryDirs pkg) exposedModule
            let mkDefs qname = Set.map ((toPackageRef pkg, exposedModule,) . origName) qname
            return (mkDefs values, mkDefs types)
     let valueTable = toLookupTable (gUnqual . trd3) valueDefs
         typeTable  = toLookupTable (gUnqual . trd3) typeDefs
     return (valueTable, typeTable)
  where
    gUnqual (OrigName _ (GName _ n))  = n


lookupDefinitions :: Map String [CanonicalSymbol] -> QName (Scoped SrcSpan) -> [CanonicalSymbol]
lookupDefinitions symbolTable qname = fromMaybe [] $
  do n <- unQName qname
     Map.lookup n symbolTable
  where
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

snd3 :: (a, b, c) -> b
snd3 (_, y, _) = y

trd3 :: (a, b, c) -> c
trd3 (_, _, z) = z
