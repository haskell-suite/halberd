module Main where

import           Control.Applicative
import           Control.Monad hiding (forM_)
import           Data.Foldable (forM_)
import           Data.List
import           Data.Proxy
import           Distribution.HaskellSuite
import           Distribution.Simple.Compiler
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names.Interfaces
import           System.Environment
import           System.Exit

import           Halberd.ChosenImports
import           Halberd.Suggestions
import           Halberd.UI

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
             [UserPackageDB, GlobalPackageDB]
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

askUserChoices :: [Suggestion] -> IO ChosenImports
askUserChoices = resolveAllSuggestions askUserChoice
