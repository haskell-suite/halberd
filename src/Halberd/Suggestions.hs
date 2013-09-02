{-# LANGUAGE DoAndIfThenElse  #-}
{-# LANGUAGE FlexibleContexts #-}
module Halberd.Suggestions where

import           Control.Applicative
import           Control.Arrow
import           Control.Monad hiding (forM_)
import           Control.Monad.State hiding (forM_)
import           Data.Function
import           Data.List
import           Data.Maybe
import qualified Data.Map                            as Map
import           Data.Monoid
import           Distribution.HaskellSuite
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names
import           Safe

import Data.Tuple.Utils
import Halberd.ChosenImports
import Halberd.CollectNames
import Halberd.LookupTable
import Halberd.Types
import Language.Haskell.Exts.Utils

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

type ChooseExternal m = QName (Scoped SrcSpan) -> [CanonicalSymbol] -> m CanonicalSymbol

resolveAllSuggestions :: (Functor m, Monad m) => ChooseExternal m -> [Suggestion] -> m ChosenImports
resolveAllSuggestions chooseExternal suggestions = execStateT (go suggestions) mempty
  where
    go sugs = do
      remaining <- resolveSuggestions sugs
      case remaining of
        [] -> return []
        ((qname, modules):ss) -> do
          choice <- lift $ chooseExternal qname modules
          modify $ insertChoice qname (snd3 choice)
          go ss

resolveSuggestions :: (Functor m, MonadState ChosenImports m) => [Suggestion] -> m [Suggestion]
resolveSuggestions suggestions =
  do newSuggestions <- resolveSuggestionsOnePass suggestions
     if suggestions == newSuggestions
     then return newSuggestions
     else resolveSuggestions newSuggestions

resolveSuggestionsOnePass :: (Functor m, MonadState ChosenImports m) => [Suggestion] -> m [Suggestion]
resolveSuggestionsOnePass suggestions = fmap catMaybes . forM suggestions $ \suggestion@(qname, modules) ->
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
