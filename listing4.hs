import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Data.Ratio
import Data.Complex

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
showVal (Bool False) = "#t"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList = unwords . map showVal

main = do
	args <- getArgs
	putStrLn (readExpr $ args !! 0)

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

--parseNumber = liftM (Number . read) $ many1 digit

--parseNumber = do num <- many1 digit
--                 return $ Number (read num)

--parseNumber = many1 digit >>= (\num -> return . Number $ read num)

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
    return $ List [Atom "Quote", x]

parseQuasiQuoted = do 
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote" , x]

parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List[Atom "unquote", x]
    
--------begin evaluator--------

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val

--------end evaluator--------

readExpr input = case parse parseExpr "lisp" input of
		Left err -> "No Match: " ++ show err
		Right val -> "Found " ++ show val

