module Main where

import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readHex, readDec, readOct)

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  deriving Show

-- String
escape :: Parser Char
escape = do
  char '\\'
  s <- oneOf "ntr\\\""
  return $ case s of
    'n' -> '\n'
    't' -> '\t'
    _ -> s

strToken :: Parser Char
strToken = escape <|> noneOf "\""

parseString :: Parser LispVal
parseString = do
  char '"'
  x <- many strToken
  char '"'
  return $ String x

-- Atom
symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space


parseAtom :: Parser LispVal
parseAtom = do
  first <- letter <|> symbol
  rest <- many (letter <|> digit <|> symbol)
  let atom = first:rest
  return $ case atom of
    "#t" -> Bool True
    "#f" -> Bool False
    _ -> Atom atom


-- Number

data NumberFormat = Hex | Dec | Oct | Default deriving Show

parseRadix :: Parser NumberFormat
parseRadix = do
  char '#'
  s <- oneOf "odx"
  return $ case s of
    'o' -> Oct
    'd' -> Dec
    'x' -> Hex

defaultRadix :: Parser NumberFormat
defaultRadix = return Default

parseNumber :: Parser LispVal
parseNumber = do
  radixL <- parseRadix <|> defaultRadix
  digits <- many1 (oneOf "abcdefABCDEF" <|> digit)
  radixR <- parseRadix <|> defaultRadix
  return $ case (radixL, radixR) of
    (Oct,_) -> let [(x,_)] = readOct digits in Number x
    (Hex,_) -> let [(x,_)] = readHex digits in Number x
    (Dec,_) -> let [(x,_)] = readDec digits in Number x
    (Default, Oct) -> let [(x,_)] = readOct digits in Number x
    (Default, Hex) -> let [(x,_)] = readHex digits in Number x
    (Default, Dec) -> let [(x,_)] = readDec digits in Number x
    (Default, Default) -> let [(x,_)] = readDec digits in Number x

-- Expr

parseExpr :: Parser LispVal
parseExpr = parseNumber <|> parseString <|> parseAtom


readExpr :: String -> String
readExpr input = case parse parseExpr "scheme" input of
  Left err -> show err
  Right val -> show val

main :: IO ()
main = do
  (expr:_) <- getArgs
  putStrLn (readExpr expr)
