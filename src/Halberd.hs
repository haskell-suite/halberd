{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ImplicitParams       #-}
{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE DoAndIfThenElse      #-}
{-# LANGUAGE FlexibleContexts     #-}

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (forM_)
import           Control.Monad.State hiding (forM_)
import           Data.Foldable (forM_)
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Map                            as Map
import           Data.Monoid
import           Data.Proxy
import           Distribution.HaskellSuite
import           Distribution.Simple.Compiler
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names
import           Language.Haskell.Names.Interfaces
import           Safe
import           System.Environment
import           System.Exit

import           Data.Tuple.Utils
import           Halberd.ChosenImports
import           Halberd.CollectNames                (collectUnboundNames)
import           Halberd.LookupTable
import           Halberd.Types
import           Halberd.UI
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
         pkgs <- concat <$> mapM (getInstalledPackages (Proxy :: Proxy NamesDB))
             [UserPackageDB, GlobalPackageDB, SpecificPackageDB "/Users/erik/doc/haskell-suite/.cabal-sandbox/x86_64-osx-haskell-names-0.1-packages.conf.d"]
         allSuggestions <- evalModuleT (suggestedImports module_) pkgs suffix readInterface

         let (suggestions, noSuggestions) = partition (not . null . snd) allSuggestions

         chosenImports <- askUserChoices suggestions

         when (not . null $ noSuggestions) $ do
           putStrLn "------------- Could not find import for -------------"
           forM_ noSuggestions $ \(q, _) -> do
             putStrLn $ " - " ++ prettyPrint q
           putStrLn ""

         when (not . isEmpty $ chosenImports) $ do
           putStrLn "-------- Insert these imports into your file --------"
           putStrLn ""
           putStrLn $ unlines (showChosenImports chosenImports)
  where
    suffix = "names"

type Suggestion = (QName (Scoped SrcSpan), [CanonicalSymbol])

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

askUserChoices :: [Suggestion] -> IO ChosenImports
askUserChoices suggestions = execStateT (go suggestions) mempty
  where
    go sugs = do
      remaining <- resolveSuggestions sugs
      case remaining of
        [] -> return []
        ((qname, modules):ss) -> do
          choice <- askUserChoice qname modules
          modify $ insertChoice qname (snd3 choice)
          go ss

resolveSuggestions :: (Functor m, MonadState ChosenImports m) => [Suggestion] -> m [Suggestion]
resolveSuggestions suggestions = fmap catMaybes . forM suggestions $ \suggestion@(qname, modules) ->
  do chosenModules <- get
     if alreadyChosen qname modules chosenModules
     then
       return Nothing
     else do
       case hasSingleOption qname modules chosenModules of
         Nothing           -> return $ Just suggestion
         Just choice -> do
           modify $ insertChoice qname (snd3 choice)
           return Nothing
  where
    alreadyChosen qname modules chosenModules = fromMaybe False $
      do q <- getQualification qname
         module_ <- lookupQualified q chosenModules
         return $ module_ `elem` map snd3 modules
    hasSingleOption _        [module_] _             = Just module_
    hasSingleOption UnQual{} modules   chosenModules | singleOrigName modules =
      headMay $ filter ((`Map.member` unqualifieds chosenModules) . snd3) modules
    hasSingleOption _        _         _             = Nothing
    singleOrigName = allEqual . map trd3
    allEqual []     = True
    allEqual (x:xs) = all (== x) xs

findUnbound :: Module SrcSpanInfo -> ModuleT Symbols IO ([QName (Scoped SrcSpan)], [QName (Scoped SrcSpan)])
findUnbound module_ = collectUnboundNames <$> annotateModule Haskell98 [] (fmap srcInfoSpan module_)
