import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Numeric

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float
             deriving Show 

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

escapedChars :: Parser String
escapedChars = do char '\\'
                  x <- oneOf "\\\"ntr"
                  case x of 
                    '\\' -> return [x]
                    '"'  -> return [x]
                    'n' -> return "\n"
                    't' -> return "\t"
                    'r' -> return "\r"

parseChar :: Parser LispVal
parseChar = do try $ string "#\\"
               x <- parseCharName <|> anyChar
               return $ Character x
 
parseCharName = do x <- try $ string "space" <|> string "newline"
                   case x of
                     "space"   -> return ' '
                     "newline" -> return '\n'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ many1 (noneOf $ "\"\\") 
                      <|> escapedChars
                 char '"'
                 return $ (String . concat) x

parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest  <- many (letter <|> digit <|> symbol)
               let atom = first : rest
               return $ Atom atom
                           
parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of 
                          'f' -> Bool False
                          't' -> Bool True
parseNum :: Parser LispVal
parseNum = do x <- parseDig <|> parseDec 
                     <|> parseHex <|> parseOct 
                     <|> parseBin
              return $ x

parseDig :: Parser LispVal
parseDig = do x <- many1 digit
              return $ (Number . read) x

parseDec :: Parser LispVal
parseDec = do try $ string "#d"
              x <- many1 digit
              return $ Number (read x)

parseHex :: Parser LispVal
parseHex = do try $ string "#h"
              x <- many1 hexDigit
              return $ Number (hex2dig x)

parseOct :: Parser LispVal
parseOct = do try $ string "#o"
              x <- many1 octDigit
              return $ Number (oct2dig x)

parseBin :: Parser LispVal
parseBin = do try $ string "#b"
              x <- many1 (oneOf "10")
              return $ Number (bin2dig x)

oct2dig :: (Eq a, Num a) => String -> a
oct2dig x = fst $ readOct x !! 0

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ readHex x !! 0

bin2dig :: [Char] -> Integer
bin2dig = bin2digH 0

bin2digH :: Num t => t -> [Char] -> t
bin2digH digint "" = digint
bin2digH digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in bin2digH old xs 

parseFloat :: Parser LispVal
parseFloat = do x <- many1 digit
                char '.'
                y <- many1 digit
                return $ (Float . read) (x ++ "." ++ y)


parseExpr :: Parser LispVal
parseExpr = parseAtom 
         <|> parseString 
         <|> try parseFloat
         <|> try parseNum
         <|> try parseBool
         <|> try parseChar

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of 
                   Left err  -> "No match: " ++ show err
                   Right val -> "Found value: " ++ show val

main :: IO ()
main = do
        (expr : _) <- getArgs
        putStrLn $ readExpr expr
