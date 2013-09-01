Halberd: generating imports
==========

Halberd is a tool to help you add missing imports to your Haskell source files. With it, you can write your source without imports, call Halberd, and just paste in the import lines.

Currently, it tries to automatically choose an import if there is a single sensible option. If it can't, it will prompt you with a simple menu. After running, it prints the imports, which you need to copy manually. Editor integration is planned.

The imports generated are either qualified, if the unbound function looks like `M.lookup`, or explicit, if it looks like `void`.

Example
----------

As an example, say you have written the following (nonsensical and type incorrect) Haskell file called `test.hs`:

``` haskell
main :: IO Int8
main = do
    forM_ [1,2] $ \x -> print $ M.lookup x table
    liftM id $ return $ headNote "Impossible" [10]

table :: M.Map String Int8
table = M.fromList [("Odeca", 1), ("Hackathon",2)]
```

You can run Halberd on this by running `halberd test.hs`. It will prompt you with three questions:

```
forM_:
1) Control.Monad
2) Data.Foldable

M.lookup:
1) Data.List
2) GHC.List
3) Prelude
4) Data.IntMap
5) Data.IntMap.Lazy
6) Data.IntMap.Strict
7) Data.Map
8) Data.Map.Lazy
9) Data.Map.Strict

Int8:
1) Data.Int
2) Foreign
3) Foreign.Safe
4) GHC.Int
```

After making the choices by typing `1`, `7` and `1`, it generated the folling output:

``` haskell
------------- Could not find import for -------------
 - headNote

-------- Insert these imports into your file --------

import qualified Data.Map as M
import Control.Monad ( forM_, liftM )
import Data.Int ( Int8 )
```

As you can see, it didn't ask questions about `M.Map`, `M.fromList`, `liftM` and the second usage of `Int8`. It figured these out either because of previous choices, or because there was only a single option.

Installation
----------

Halberd uses the Haskell Suite packages (`haskell-src-exts`, `haskell-packages` and `haskell-names`) for parsing, name resolution and finding exposed identifiers of packages. While these can be installed from hackage, generating databases of names for new packages currently needs a custom version of the `cabal install` executable.  See the [documentation for haskell-names](https://github.com/haskell-suite/haskell-names) for more details.

To install Halberd, just do

```
cabal install halberd
```

This will give you a `halberd` executable that takes a single argument, the file to generate imports for. By default, it only draws names from `base`. If you want to add more names, use `hs-gen-iface` from the `haskell-names` package.

Improvements
----------

Halberd is still in an unfinished state. Some planned improvements are:

* Smarter automatic choices.
* Editor integration.
* Easier installation of name databases (`haskell-names`).
* Integration with existing imports.
* Better choice UI.
* Choices based on type information (`haskell-type-exts`).

Contributions are welcome!
