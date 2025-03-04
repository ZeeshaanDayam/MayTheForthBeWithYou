module Main where

-- Running: runhaskell Main.hs path_to_test_file

import Interpret
import System.Environment

main :: IO ()
main = do
    (fileName:_) <- getArgs
    contents <- readFile fileName
    let (stack, output) = interpret contents
    putStrLn output  
    if null stack
        then return ()
        else do
            putStrLn "Warning: Stack is not empty after execution!"
            print stack
