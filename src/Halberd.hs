import Control.Applicative
import Distribution.HaskellSuite.Helpers
import Distribution.HaskellSuite.Tool
import Distribution.Simple.Compiler
import Language.Haskell.Exts.Annotated
import Language.Haskell.Modules
import Language.Haskell.Modules.Interfaces
import qualified Data.Map as Map
import System.FilePath
import Text.Show.Pretty

main =
  do (ParseOk mod) <- parseFile "test.hs"
     pkgs <- concat <$> mapM (toolGetInstalledPkgs theTool) [UserPackageDB, GlobalPackageDB]
     info <- evalModuleT (analyseModules [fmap srcInfoSpan mod]) pkgs retrieveModuleInfo Map.empty
     putStrLn (ppShow info)

-- This function says how we actually find and read the module
-- information, given the search path and the module name
-- retrieveModuleInfo :: [FilePath] -> ModuleName -> IO Symbols
retrieveModuleInfo dirs name = do
  (base, rel) <- findModuleFile dirs [suffix] name
  readInterface $ base </> rel

theTool :: SimpleTool
theTool =
  simpleTool
    "haskell-modules"
    undefined
    knownExtensions
    (return Nothing)
    undefined
    [suffix]

suffix = "names"
