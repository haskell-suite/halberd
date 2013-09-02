module Halberd.UI where

import           Control.Applicative
import           Control.Monad.State hiding (forM_)
import           Data.Foldable (forM_)
import qualified Distribution.Text                   as Cabal
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names
import           Safe
import           System.IO

import Halberd.Types

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
