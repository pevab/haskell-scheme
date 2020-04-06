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

--parseNumber :: Parser LispVal
--parseNumber = liftM (Number . read) $ many1 digit
--
--parseNumber2 :: Parser LispVal
--parseNumber2 = do
--  s <- many1 digit
--  let n = read s
--  return $ Number n
--
--parseNumber3 :: Parser LispVal
--parseNumber3 = (many1 digit) >>= \s -> return $ Number (read s)

data NumberFormat = Hex | Dec | Oct deriving Show

parseRadix :: Parser NumberFormat
parseRadix = do
  char '#'
  s <- oneOf "odx"
  return $ case s of
    'o' -> Oct
    'd' -> Dec
    'x' -> Hex

defaultRadix :: Parser NumberFormat
defaultRadix = return Dec

parseNumber :: Parser LispVal
parseNumber = do
  r <- parseRadix <|> defaultRadix
  n <- many1 (letter <|> digit)
  return $ case r of
    Oct -> let [(x,_)] = readOct n in Number x
    Hex -> let [(x,_)] = readHex n in Number x
    Dec -> let [(x,_)] = readDec n in Number x


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
