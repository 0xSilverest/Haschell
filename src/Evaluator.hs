module Evaluator where

import LispVal
import LispError
import Control.Monad.Except (throwError)
import Data.Functor

eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "if", pred, conseq, alt]) =  eval pred >>= ifEval
                     where 
                         ifEval pred = 
                             case pred of
                               Bool False -> eval alt
                               _  -> eval conseq

eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval errForm = throwError $ BadSpecialForm  "Unrecognized special form" errForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop (*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem),
              ("not", unaryOp notp),
              ("symbol?", unaryOp isSym),
              ("string?", unaryOp isStr),
              ("number?", unaryOp isNum),
              ("bool?", unaryOp isBool),
              ("list?", unaryOp isList),
              ("symbol->string", unaryOp sym2str),
              ("string->symbol", unaryOp str2sym),
              ("=", numBoolBinop(==)),
              ("<", numBoolBinop(<)),
              (">", numBoolBinop(>)),
              ("/=", numBoolBinop(/=)),
              (">=", numBoolBinop(>=)),
              ("<=", numBoolBinop(<=)),
              ("&&", boolBoolBinop(>=)),
              ("||", boolBoolBinop(<=)),
              ("string=?", strBoolBinop(==)),
              ("string<?", strBoolBinop(<)),
              ("string>?", strBoolBinop(<)),
              ("string<=?", strBoolBinop(<=)),
              ("string>=?", strBoolBinop(>=))]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op args = Number . foldl1 op <$> mapM unpackNum args 

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp p [arg] = return $ p arg 

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 then
                                throwError $ NumArgs 2 args
                             else
                                do left <- unpacker $ head args
                                   right <- unpacker $ head args
                                   return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError Integer 
unpackNum (Number n) = return n
unpackNum _ = return 0

unpackBool :: LispVal -> ThrowsError Bool
unpackBool (Bool b) = return b
unpackBool notBool = throwError $ TypeMismatch "boolean" notBool

unpackStr :: LispVal -> ThrowsError String
unpackStr (String str) = return str
unpackStr (Number n) = return $ show n
unpackStr (Bool b) = return $ show b
unpackStr notString = throwError $ TypeMismatch "string" notString

-- Type Checkers
isSym, isNum, isStr, isBool, isList :: LispVal -> LispVal
isSym (Atom _) = Bool True
isSym _ = Bool False
isStr (String _) = Bool True
isStr _ = Bool False
isNum (Number _) = Bool True
isNum _ = Bool False
isList (List _) = Bool True
isList (DottedList _ _) = Bool False
isList _ = Bool False
isBool (Bool _) = Bool True
isBool _ = Bool False

sym2str, str2sym :: LispVal -> LispVal
sym2str (Atom val) = String val
sym2str _ = String ""
str2sym (String val) = Atom val
str2sym _ = Atom ""

notp :: LispVal -> LispVal
notp (Bool False) = Bool True
notp _ = Bool False



