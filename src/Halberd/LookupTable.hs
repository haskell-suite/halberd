{-# LANGUAGE TupleSections #-}
module Halberd.LookupTable where

import           Control.Arrow
import           Control.Monad hiding (forM_)
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Map                            (Map)
import qualified Data.Map                            as Map
import           Data.Monoid
import           Data.Ord
import           Data.Set                            (Set)
import qualified Data.Set                            as Set
import           Distribution.HaskellSuite
import qualified Distribution.InstalledPackageInfo   as Cabal
import           Language.Haskell.Exts.Annotated
import           Language.Haskell.Names

import Data.Tuple.Utils
import Halberd.Types

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


lookupDefinitions :: LookupTable -> QName (Scoped SrcSpan) -> [CanonicalSymbol]
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
