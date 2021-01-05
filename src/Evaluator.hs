module Evaluator where

import LispVal

eval :: LispVal -> LispVal
eval val@(String _) = val
eval val@(Number _) = val
eval val@(Bool _) = val
eval (List [Atom "quote", val]) = val
eval (List (Atom func : args)) = apply func $ map eval args

apply :: String -> [LispVal] -> LispVal
apply func args = maybe (Bool False) ($ args) $ lookup func primitives

primitives :: [(String, [LispVal] -> LispVal)]
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
              ("string->symbol", unaryOp str2sym)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> LispVal
numericBinop op args = Number $ foldl1 op $ map unpack args

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> LispVal
unaryOp p [arg] = p arg 

unpack :: LispVal -> Integer 
unpack (Number n) = n
unpack _ = 0

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
