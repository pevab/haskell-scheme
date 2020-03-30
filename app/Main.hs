module Main where
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  let first = read (args !! 0)::Int
      second = read (args !! 1)::Int
      in putStrLn ("Hello, " ++ show (first + second))
