import LispVal
import Text.ParserCombinators.Parsec hiding (spaces)
import Numeric
import Data.Functor ((<$>))
import Data.Ratio
import Data.Complex

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
                 x <- many $ many1 (noneOf "\"\\") 
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
parseNum = do parseDig 
              <|> parseDec 
              <|> parseHex
              <|> parseOct 
              <|> parseBin

parseDig :: Parser LispVal
parseDig = do try $ string ""
              x <- many1 digit
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
oct2dig x = fst $ head (readOct x)

hex2dig :: (Eq a, Num a) => String -> a
hex2dig x = fst $ head (readHex x)

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

parseRatio :: Parser LispVal
parseRatio = do x <- many1 digit
                char '/'
                y <- many1 digit
                return $ Rational (read x % read y)

toDouble :: LispVal -> Float
toDouble(Float f) = realToFrac f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do try $ string ""
                  x <- numberParser
                  char '+'
                  y <- numberParser
                  char 'i'
                  return $ Complex (toDouble x :+ toDouble y)
                      where 
                          numberParser = try parseFloat <|> try parseNum

parseList :: Parser LispVal
parseList = sepBy parseExpr spaces >>= return . List

parseDottedList :: Parser LispVal
parseDottedList = do head <- endBy parseExpr spaces
                     tail <- char '.' >> spaces >> parseExpr 
                     return $ DottedList head tail           

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseQuasiquote :: Parser LispVal
parseQuasiquote = do char '`'
                     x <- parseExpr
                     return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do char ','
                  x <- parseExpr
                  return $ List [Atom "unquote", x]

parseQuoteTypes :: Parser LispVal
parseQuoteTypes = parseQuoted <|> parseUnquote 
                <|> parseQuasiquote

parseListTypes :: Parser LispVal
parseListTypes = do char '('
                    x <- try parseList <|> parseDottedList
                    char ')'
                    return x

parseExpr :: Parser LispVal
parseExpr = parseAtom 
         <|> parseString 
         <|> try parseComplex
         <|> try parseRatio
         <|> try parseFloat
         <|> try parseNum
         <|> try parseBool
         <|> try parseChar
         <|> parseQuoteTypes
         <|> parseListTypes

