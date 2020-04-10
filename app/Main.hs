module Main where

import System.Environment
import SchemeEval (readExpr, eval)

main :: IO ()
main = getArgs >>= print . eval . readExpr . head
