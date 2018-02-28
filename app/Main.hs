module Main where

import Control.Monad
import SimpleScript.Parser

main :: IO ()
main = forever (getLine >>= (statement #))
