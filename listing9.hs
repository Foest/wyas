{-# LANGUAGE ExistentialQuantification #-}
import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex
import Control.Monad.Error
import System.IO
import Data.IORef

main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> runOne $ args !! 0
            otherwise -> putStrLn "Program takes only 0 or 1 argument"

readExpr input = case parse parseExpr "lisp" input of
		Left err -> throwError $ Parser err
		Right val -> return val

-------------begin variables--------------
type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ErrorT LispError IO

liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runErrorT (trapError action) >>= return . extractValue

isBound envRef var = readIORef envRef >>= return . maybe False (const True) .lookup var

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do env <- liftIO $ readIORef envRef
                       maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                             (liftIO . readIORef)
                             (lookup var env)

setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar " Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value

defineVar envRef var value = do
    alreadyDefined <- liftIO $ isBound envRef var
    if alreadyDefined
       then setVar envRef var value >> return value
       else liftIO $ do
          valueRef <- newIORef value
          env <- readIORef envRef
          writeIORef envRef ((var, valueRef) : env)
          return value

bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
    where extendEnv bindings env = liftM (++env) (mapM addBinding bindings)
          addBinding (var, value) = do ref <- newIORef value
                                       return (var, ref)


-------------end variables--------------

data LispVal =	Atom String
		| List [LispVal]
		| DottedList [LispVal] LispVal
		| Number Integer
		| String String
		| Bool Bool
		| Character Char
		| Float Double
		| Ratio Rational
		| Complex (Complex Double)
                | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
                | Func {params :: [String], vararg :: (Maybe String),
                         body :: [LispVal], closure :: Env}
--                deriving (Eq)

instance Show LispVal where show = showVal

showVal :: LispVal -> String
--HFW addition
showVal (Character char) = "#\\" ++ [char]
--HFW addition
showVal (Complex contents) = show contents
--HFW addition
showVal (Float contents) = show contents
--HFW addition
showVal (Ratio contents) = show contents
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal (Func {params = args, vararg = varargs, body = body, closure = env}) =
  "(lambda (" ++ unwords (map show args) ++
     (case varargs of
        Nothing -> ""
        Just arg -> " . " ++ arg) ++ ") ...)"

unwordsList = unwords . map showVal

data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

symbol = oneOf "!$%&|*+-/<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escChars :: Parser Char
escChars = do char '\\'
              x <- oneOf "\\\"nrt"
              return $ case x of
                       '\\' -> x
                       '\"' -> x
                       'n'  -> '\n'
                       'r'  -> '\r'
                       't'  -> '\t'

parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                        't' -> Bool True
                        'f' -> Bool False

parseCharacter = do try $ string "#\\"
                    val <- try (string "newline" <|> string "space")
                           <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
                    return $ Character $ case val of
                                         "space" -> ' '
                                         "newline" -> '\n'
                                         otherwise -> (val !! 0)

parseString = do char '"'
                 x <- many (escChars <|> noneOf "\"\\")
                 char '"'
                 return $ String x

parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               let atom = first:rest
               return $ case atom of
                        "#t" -> Bool True
                        "#f" -> Bool False
                        _    -> Atom atom


parseNumber = do num <- parseDigital1 <|> parseDigital2 <|> parseHex <|> parseOct <|> parseBin
                 return $ num

{-
--different versions of parseNumber
parseNumber = liftM (Number . read) $ many1 digit

parseNumber = do num <- many1 digit
                 return $ Number (read num)

parseNumber = many1 digit >>= (\num -> return . Number $ read num)
-}

parseDigital1 = do x <- many1 digit
                   (return . Number . read) x

parseDigital2 = do try $ string "#d"
                   x <- many digit
                   (return . Number . read) x

parseHex = do try $ string "#x"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig x = fst $ readOct x !! 0
hex2dig x = fst $ readHex x !! 0
bin2dig = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1)
                         in bin2dig' old xs

parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ Float (fst . head $ readFloat (x ++ "." ++ y))

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Ratio ((read x) % (read y))

parseComplex = do x <- (try parseFloat <|> parseRatio)
                  char '+'
                  y <- (try parseFloat <|> parseRatio)
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)

toDouble (Float f) = f
toDouble (Number n) = fromIntegral n
--HFW addition
toDouble (Ratio r) = fromRational r

parseExpr = parseAtom
        <|> parseString
	<|> try parseComplex
	<|> try parseFloat
	<|> try parseRatio
	<|> try parseNumber
	<|> try parseBool
	<|> try parseCharacter
	<|> parseQuoted
	<|> parseQuasiQuoted
	<|> parseUnQuote
	<|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x

parseList = liftM List $ sepBy parseExpr spaces

parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote" , x]

parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List[Atom "unquote", x]

--------begin evaluator--------

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "quote", val]) = return val

--evaluation of if expression
eval env (List [Atom "if", pred, conseq, alt]) =
    do result <- eval env pred
       case result of
         Bool False -> eval env alt
         Bool True -> eval env conseq
         _ -> throwError $ TypeMismatch "bool" pred

eval env (List [Atom "set!", Atom var, form]) =
    eval env form >>= setVar env var

eval env (List [Atom "define", Atom var, form]) =
    eval env form >>= defineVar env var

eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var

eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) =
    makeVarargs varargs env params body >>= defineVar env var

eval env (List (Atom "lambda" : List params : body)) =
    makeNormalFunc env params body

eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarargs varargs env params body

eval env (List (Atom "lambda" : varargs@(Atom _) : body)) =
    makeVarargs varargs env [] body

eval env (List (function : args)) = do
    func <- eval env function
    argVals <- mapM (eval env) args
    apply func argVals

{-
--evaluation of case expression
eval env form@(List (Atom "case" : key : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clauses in case expression" form
  else case head clauses of
    List (Atom "else" : exprs) -> mapM (eval env) exprs >>= return . last
    List ((List datums) : exprs) -> do
      result <- eval env key
      equality <- mapM (\x -> eqv [result, x]) datums
      if Bool True `elem` equality
        then mapM (eval env) exprs >>= return . last
        else (eval env) $ List (Atom "case" : key : tail clauses)
    _ -> throwError $ BadSpecialForm "ill-formed case expression:" form
-}



--evaluation of cond expression
eval env form@(List (Atom "cond" : clauses)) =
  if null clauses
  then throwError $ BadSpecialForm "no true clause in cond expression:" form
  else case head clauses of
    List [Atom "else", expr] -> eval env expr
    List [test, expr] -> (eval env) $ List [Atom "if",
                                      test,
                                      expr,
                                      List (Atom "cond" : tail clauses)]
    _ -> throwError $ BadSpecialForm "ill-formed cond expression:" form

--eval env (List (Atom func : args)) = mapM (eval env) args >>= liftThrows . apply func
eval env badForm = throwError $ BadSpecialForm "Unrecognized special form " badForm



apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftThrows $ func args

apply (Func params varargs body closure) args =
    if num params /= num args && varargs == Nothing
       then throwError $ NumArgs (num params) args
       else (liftIO $ bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
    where remainingArgs = drop (length params) args
          num = toInteger . length
          evalBody env = liftM last $ mapM (eval env) body
          bindVarArgs arg env = case arg of
              Just argName -> liftIO $ bindVars env [(argName, List $ remainingArgs)]
              Nothing -> return env



primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
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
              ("symbol?", unaryOp symbolp),
              ("string?", unaryOp stringp),
              ("number?", unaryOp numberp),
              ("bool?", unaryOp boolp),
              ("list?", unaryOp listp),
              ("car", car),
              ("cdr", cdr),
              ("cons", cons),
              ("eq?", eqv),
              ("eqv?", eqv),
              ("equal?", equal)]

primitiveBindings :: IO Env
primitiveBindings = nullEnv >>= (flip bindVars $ map makePrimitiveFunc primitives)
    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

unaryOp f [v] = return $ f v
unaryOp _ _ = throwError $ Default "unaryOp Error"

symbolp (Atom _) = Bool True
symbolp _ = Bool False

numberp (Number _) = Bool True
numberp _ = Bool False

stringp (String _) = Bool True
stringp _ = Bool False

boolp (Bool _) = Bool True
boolp _ = Bool False

listp (List _) = Bool True
listp (DottedList _ _) = Bool True
listp _ = Bool False

numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2
                             then throwError $ NumArgs 2 args
                             else do left <- unpacker $ args !! 0
                                     right <- unpacker $ args !! 1
                                     return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum

strBoolBinop = boolBinop unpackStr

boolBoolBinop = boolBinop unpackBool

unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                          if null parsed
                            then throwError $ TypeMismatch "number" $ String n
                            else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

unpackStr (String s) = return s
unpackStr (Number s) = return $ show s
unpackStr (Bool s) = return $ show s
unpackStr notString = throwError $ TypeMismatch "string" notString

unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

symbol2string (Atom s) = String s
symbol2string _ = String ""
string2symbol (String s) = Atom s
string2symbol _ = Atom ""

car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons [x1, List []] = return $ List [x1]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs xlast] = return $ DottedList (x : xs) xlast
cons [x1, x2] = return $ DottedList [x1] x2
cons badArgList = throwError $ NumArgs 2 badArgList

eqv [(Bool arg1), (Bool arg2)] = return $ Bool $ arg1 == arg2
eqv [(Number arg1), (Number arg2)] = return $ Bool $ arg1 == arg2
eqv [(String arg1), (String arg2)] = return $ Bool $ arg1 == arg2
eqv [(Atom arg1), (Atom arg2)] = return $ Bool $ arg1 == arg2
eqv [(DottedList xs x), (DottedList ys y)] = eqv [List $ xs ++ [x], List $ ys ++ [y]]
eqv [l1@(List arg1), l2@(List arg2)] = eqvList eqv [l1, l2]

{-
eqv [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqv [x1, x2] of
                               Left err -> False
                               Right (Bool val) -> val
-}

eqv [_, _] = return $ Bool False
eqv badArgList = throwError $ NumArgs 2 badArgList

eqvList eqvFunc [(List arg1), (List arg2)] = return $ Bool $ (length arg1 == length arg2) &&
                                                    (all eqvPair $ zip arg1 arg2)
    where eqvPair (x1, x2) = case eqvFunc [x1, x2] of
                                  Left err -> False
                                  Right (Bool val) -> val

equal [l1@(List arg1), l2@(List arg2)] = eqvList equal [l1, l2]
equal [(DottedList xs x), (DottedList ys y)] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [arg1, arg2] = do
    primitiveEquals <- liftM or $ mapM (unpackEquals arg1 arg2)
                      [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
    eqvEquals <- eqv [arg1, arg2]
    return $ Bool $ (primitiveEquals || let (Bool x) = eqvEquals in x)
equal badArgList = throwError $ NumArgs 2 badArgList

unpackEquals arg1 arg2 (AnyUnpacker unpacker) =
             do unpacked1 <- unpacker arg1
                unpacked2 <- unpacker arg2
                return $ unpacked1 == unpacked2
        `catchError` (const $ return False)

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env

makeNormalFunc = makeFunc Nothing

makeVarargs = makeFunc . Just . showVal

--------end evaluator--------


--------begin error checking-------

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where show = showError

instance Error LispError where
    noMsg = Default "An error has occured"
    strMsg = Default

type ThrowsError = Either LispError

showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) =  "Expected " ++ show expected
                                  ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

trapError action = catchError action (return . show)

extractValue (Right val) = val

--------end error checking--------


--------begin REPL--------

runRepl = until_ (== "quit") (readPrompt "Lisp>>> ") evalAndPrint

--REPL helper functions
flushStr str = putStr str >> hFlush stdout

readPrompt prompt = flushStr prompt >> getLine

evalString expr = return $ extractValue $ trapError (liftM show $ readExpr expr >>= eval)

evalAndPrint expr = evalString expr >>= putStrLn

until_ pred prompt action = do
  result <- prompt
  if pred result
    then return ()
    else action result >> until_ pred prompt action

--------end REPL--------
