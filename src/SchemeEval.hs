module SchemeEval where
import SchemeModel
import SchemeParser (parseExpr)
import Text.ParserCombinators.Parsec hiding (spaces)

-- display

showVal :: LispVal -> String
showVal (String s) = "String \"" ++ s ++ "\""
showVal (Atom a) = "Atom " ++ a
showVal (Number n) = "Number " ++ show n
showVal (Bool True) = "Bool #t"
showVal (Bool False) = "Bool #f"

showVal (List l) = "List (" ++ unwordsList l ++ ")"
showVal (DottedList h t) = "Dotted List (" ++ unwordsList h ++ " . " ++ showVal t ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

showType :: LispVal -> String
showType (String _) = "String"
showType (Atom _) = "Atom"
showType (Number _) = "Number"
showType (Bool _) = "Bool"
showType (List _) = "List"
showType (DottedList _ _) = "DottedList"

instance Show LispVal where show = showVal

readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> String $ "No match: " ++ show err
  Right val -> val

-- eval

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval val@(Atom _) = val
eval (List [Atom "quote", val]) = val

eval (List (Atom f : args)) = apply f $ map eval args

apply :: String -> [LispVal] -> LispVal
apply f args = maybe (Bool False) ($ args) $ lookup f primitives

primitives :: [(String, [LispVal] -> LispVal)]
primitives = [("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("string?", isString),
  ("symbol?", isAtom),
  ("symbol->string", symbolToString)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op params = Number $ foldl1 op $ map unpackNumber params

unpackNumber :: LispVal -> Integer
unpackNumber (Number n) = n
unpackNumber _ = 0

isString :: [LispVal] -> LispVal
isString [String s] = Bool True
isString (String s : tail) = isString tail
isString _ = Bool False


isAtom :: [LispVal] -> LispVal
isAtom [Atom s] = Bool True
isAtom ((Atom s) : tail) = isAtom tail
isAtom _ = Bool False

symbolToString :: [LispVal] -> LispVal
symbolToString [Atom s] = String s
symbolToString [Atom "quote", Atom s] = String s
symbolToString _ = String "error"
