module LispVal where

import Data.Complex
import Data.IORef (IORef,)
import Control.Monad.Except (ExceptT, catchError)
import Text.ParserCombinators.Parsec (Parser, ParseError)

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Float Float
             | Rational Rational 
             | Complex (Complex Float)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func {params :: [String], vararg :: Maybe String,
                     body :: [LispVal], closure :: Env}

data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Character c) = show c
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"
showVal (PrimitiveFunc _) = "<primitive>"
showVal Func {params = args, vararg = varargs, body = body, closure = env} = 
    "(lambda (" ++ unwords (map show args) ++ 
        (case varargs of
           Nothing -> ""
           Just arg -> " . " ++ arg) ++ ") ...)" 

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where 
    show = showVal

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args: found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseError) = "Parse error at " ++ show parseError

instance Show LispError where
    show = showError

type ThrowsError = Either LispError 

-- Monadic Excpetion => (Error, Valid)
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

-- Variable definition environment
type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

