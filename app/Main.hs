module Main where

import System.Environment
import SchemeParser (readExpr)

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
