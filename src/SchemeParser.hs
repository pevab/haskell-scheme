module SchemeParser where

import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad
import Numeric (readHex, readDec, readOct)

data LispVal = Atom String
  | List [LispVal]
  | DottedList [LispVal] LispVal
  | Number Integer
  | String String
  | Bool Bool
  | Character Char
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
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

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
  digits <- many1 hexDigit
  radixR <- parseRadix <|> defaultRadix
  return $ case (radixL, radixR) of
    (Oct,_) -> let [(x,_)] = readOct digits in Number x
    (Hex,_) -> let [(x,_)] = readHex digits in Number x
    (Dec,_) -> let [(x,_)] = readDec digits in Number x
    (Default, Oct) -> let [(x,_)] = readOct digits in Number x
    (Default, Hex) -> let [(x,_)] = readHex digits in Number x
    (Default, Dec) -> let [(x,_)] = readDec digits in Number x
    (Default, Default) -> let [(x,_)] = readDec digits in Number x

-- Character

parseSpaceCharacter :: Parser LispVal
parseSpaceCharacter = do
  string "#\\space"
  return $ Character ' '

parseTabCharacter :: Parser LispVal
parseTabCharacter = do
  string "#\\tab"
  return $ Character '\t'

parseNewlineCharacter :: Parser LispVal
parseNewlineCharacter = do
  string "#\\newline"
  return $ Character '\n'

parseRegularCharacter :: Parser LispVal
parseRegularCharacter = do
  string "#\\"
  c <- letter
  return $ Character c

parseCharacter :: Parser LispVal
parseCharacter = try parseSpaceCharacter
  <|> try parseTabCharacter
  <|> parseRegularCharacter

-- Float

-- List

parseList :: Parser LispVal
parseList = liftM List $ sepBy parseExpr spaces

parseDottedList :: Parser LispVal
parseDottedList = do
  head <- endBy parseExpr spaces
  tail <- char '.' >> spaces >> parseExpr
  return $ DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do
  char '\''
  x <- parseExpr
  return $ List [Atom "quote", x]

-- Expr

parseExpr :: Parser LispVal
parseExpr = parseAtom
  <|> parseString
  <|> parseNumber
  <|> parseQuoted
  <|> do
    char '('
    x <- try parseList <|> parseDottedList
    char ')'
    return x


readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
  Left err -> show err
  Right val -> show val
