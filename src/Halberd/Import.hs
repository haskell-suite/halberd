module Halberd.Import where

import Control.Arrow
import Control.Monad
import Data.List
import Data.Ord
import Language.Haskell.Exts.Annotated
import qualified Distribution.ModuleName as Cabal
import qualified Distribution.Text       as Cabal

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

toImport :: QName a -> Cabal.ModuleName -> Import
toImport qname modName =
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
