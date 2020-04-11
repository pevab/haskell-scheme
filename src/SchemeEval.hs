module SchemeEval where
import SchemeModel
import SchemeParser (parseExpr)
import Text.ParserCombinators.Parsec hiding (spaces)
import Control.Monad.Except

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


-- Error

data LispError = NumArgs Integer [LispVal]
  | TypeMismatch String LispVal
  | Parser ParseError
  | BadSpecialForm String LispVal
  | NotFunction String String
  | UnboundVar String String
  | Default String

showError :: LispError -> String
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ func
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (Default message) = message

instance Show LispError where show = showError

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Read Expression

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ Parser err
  Right val -> return val

-- eval

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply f args = maybe (throwError $ NotFunction "Unrecognized primitive function" f)
                     ($ args)
                     (lookup f primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
  ("-", numericBinop (-)),
  ("*", numericBinop (*)),
  ("/", numericBinop div),
  ("mod", numericBinop mod),
  ("quotient", numericBinop quot),
  ("remainder", numericBinop rem),
  ("string?", isString),
  ("symbol?", isAtom),
  ("symbol->string?", symbolToString)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNumber params >>= return . Number . foldl1 op

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) = let parsed = reads n in
                          if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ parsed !! 0
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum@(_) = throwError $ TypeMismatch "number" notNum

isString :: [LispVal] -> ThrowsError LispVal
isString [String s] = return $ Bool True
isString (String s : tail) = isString tail
isString _ = return $ Bool False


isAtom :: [LispVal] -> ThrowsError LispVal
isAtom [Atom s] = return $ Bool True
isAtom ((Atom s) : tail) = isAtom tail
isAtom _ = return $ Bool False

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [] = throwError $ NumArgs 1 []
symbolToString [val] = symToStr val
symbolToString bad = throwError $ NumArgs 1 bad

symToStr :: LispVal -> ThrowsError LispVal
symToStr (Atom s) = return $ String s
symToStr (List [Atom "quote", Atom s]) = return $ String s
symToStr badForm = throwError $ TypeMismatch "symbol" badForm
