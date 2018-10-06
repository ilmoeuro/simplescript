module Main where

import Control.Monad
import SimpleScript.Parser
import SimpleScript.CodeGen
import SimpleScript.Bytecode

main :: IO ()
main = forever $ do
    result <- toAssembly . codegen . parse sourceFile <$> getLine
    putStrLn result