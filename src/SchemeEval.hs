{-# LANGUAGE ExistentialQuantification #-}

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
eval (List [Atom "if", pred, conseq, alt]) = do
  result <- eval pred
  case result of
    Bool True -> eval conseq
    Bool False -> eval alt
    notBool -> throwError $ TypeMismatch "bool" notBool
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval (List a) = liftM List $ mapM eval a
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
  ("symbol->string?", symbolToString),
  ("=", numBoolBinop (==)),
  ("<", numBoolBinop (<)),
  (">", numBoolBinop (>)),
  ("/=", numBoolBinop (/=)),
  (">=", numBoolBinop (>=)),
  ("<=", numBoolBinop (<=)),
  ("&&", boolBoolBinop (&&)),
  ("||", boolBoolBinop (||)),
  ("string=?", strBoolBinop (==)),
  ("string<?", strBoolBinop (<)),
  ("string>?", strBoolBinop (>)),
  ("string<=?", strBoolBinop (<=)),
  ("string>=?", strBoolBinop (>=)),
  ("car", car),
  ("cdr", cdr),
  ("cons", cons),
  ("eq?", eqv),
  ("eqv?", eqv),
  ("equal?", equal),
  ("cond", cond),
  ("case", myCase)]

-- numeric binary op

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op [] = throwError $ NumArgs 2 []
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNumber params >>= return . Number . foldl1 op

-- bool binary op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do
                              left <- unpacker $ args !! 0
                              right <- unpacker $ args !! 1
                              return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNumber
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

-- unpackers

unpackNumber :: LispVal -> ThrowsError Integer
unpackNumber (Number n) = return n
unpackNumber (String n) = let parsed = reads n in
                          if null parsed
                          then throwError $ TypeMismatch "number" $ String n
                          else return $ fst $ parsed !! 0
unpackNumber (List [n]) = unpackNumber n
unpackNumber notNum@(_) = throwError $ TypeMismatch "number" notNum

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "bool" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

-- string?

isString :: [LispVal] -> ThrowsError LispVal
isString [String s] = return $ Bool True
isString (String s : tail) = isString tail
isString _ = return $ Bool False

-- symbol?

isAtom :: [LispVal] -> ThrowsError LispVal
isAtom [Atom s] = return $ Bool True
isAtom ((Atom s) : tail) = isAtom tail
isAtom _ = return $ Bool False

-- symbol->string?

symbolToString :: [LispVal] -> ThrowsError LispVal
symbolToString [] = throwError $ NumArgs 1 []
symbolToString [val] = symToStr val
symbolToString bad = throwError $ NumArgs 1 bad

symToStr :: LispVal -> ThrowsError LispVal
symToStr (Atom s) = return $ String s
symToStr (List [Atom "quote", Atom s]) = return $ String s
symToStr badForm = throwError $ TypeMismatch "symbol" badForm

-- car

car :: [LispVal] -> ThrowsError LispVal
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- cdr

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x:xs)] = return $ List xs
cdr [DottedList (x:[]) last] = return last
cdr [DottedList (x:xs) last] = return $ DottedList xs last
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

-- cons

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List []] = return $ List [x]
cons [x, List y] = return $ List $ x:y
cons [x, DottedList y last] = return $ DottedList (x:y) last
cons [x,y] = return $ DottedList [x] y
cons badArgList = throwError $ NumArgs 2 badArgList

-- eqv

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool a, Bool b] = return $ Bool $ a == b
eqv [String a, String b] = return $ Bool $ a == b
eqv [Number a, Number b] = return $ Bool $ a == b
eqv [Character a, Character b] = return $ Bool $ a == b
eqv [Atom a, Atom b] = return $ Bool $ a == b
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [(List x), (List y)] = return $ Bool $ (length x == length y) && (all eqvPair $ zip x y)
    where eqvPair (a, b) = case eqv [a, b] of Right (Bool val) -> val
                                              Left err -> False
eqv [_,_] = return $ Bool False
eqv badList = throwError $ NumArgs 2 badList

-- eq

data Unpacker = forall a. Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEquals :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEquals arg1 arg2 (AnyUnpacker unpacker) = do
    left <- unpacker arg1
    right <- unpacker arg2
    return $ left == right
  `catchError` (const $ return False)

equal :: [LispVal] -> ThrowsError LispVal
equal [(List a), (List b)] = return $ Bool $ (length a == length b) && (all equalPair $ zip a b)
  where equalPair (u, v) = case equal [u,v] of Right (Bool val) -> val
                                               Left err -> False
equal [arg1, arg2] = do
  primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
    [AnyUnpacker unpackNumber, AnyUnpacker unpackBool, AnyUnpacker unpackStr]
  eqvEquals <- eqv [arg1, arg2]
  return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badList = throwError $ NumArgs 2 badList


-- cond

cond :: [LispVal] -> ThrowsError LispVal
cond ((List ((Atom "else"):expressions)):_) = liftM last $ mapM eval $ expressions
cond ((List (test:(Atom "=>"):expressions)):rest) = do
  result <- liftM last $ mapM eval $ expressions
  testValue <- eval test
  case testValue of
    Bool True -> return result
    Bool False -> cond rest
    notBool -> throwError $ TypeMismatch "bool" notBool

cond ((List (test:expressions)):rest) = do
  testValue <- eval test
  case testValue of
    Bool True -> liftM last $ mapM eval $ expressions
    Bool False -> cond rest
    notBool -> throwError $ TypeMismatch "bool" notBool

-- case

myCase :: [LispVal] -> ThrowsError LispVal
myCase (key:clauses) = do
  keyValue <- eval key
  auxMyCase keyValue clauses

auxMyCase :: LispVal -> [LispVal] -> ThrowsError LispVal
auxMyCase keyValue (clause:clauses) = case clause of
  List ((List set):expressions) -> do
    ok <- liftM or $ mapM (eqp keyValue) $ set
    if ok
    then foldM (\z -> eval) (Atom "none") expressions
    else auxMyCase keyValue clauses
  List ((Atom "else"):expressions) -> foldM (\z -> eval) (Atom "none") expressions
  badForm -> throwError $ BadSpecialForm "Unrecognized special form" badForm
auxMyCase keyCalue [] = throwError $ Default "no matching cases"

eqp :: LispVal -> LispVal -> ThrowsError Bool
eqp a b = do
  v <- eqv [a,b]
  case v of
    Bool val -> return val
    notBool -> throwError $ TypeMismatch "bool" notBool