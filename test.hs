{-# LANGUAGE TemplateHaskell #-}
import Prelude


main :: BIO a
main = forM_ [1,2] print
