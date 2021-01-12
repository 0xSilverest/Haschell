{-# LANGUAGE ExistentialQuantification #-}

module Evaluator where

import LispVal
import VarEnv
import Control.Monad.Except (throwError, catchError, liftIO)
import Data.Functor ((<$>))
import Data.Maybe (isNothing)

eval :: Env -> LispVal -> IOThrowsError LispVal
eval env val@(String _) = return val
eval env val@(Number _) = return val
eval env val@(Bool _) = return val
eval env val@(Character _) = return val
eval env (Atom id) = getVar env id
eval env (List [Atom "define", Atom var, val]) = eval env val >>= defineVar env var
eval env (List (Atom "define" : List (Atom var : params) : body)) =
    makeNormalFunc env params body >>= defineVar env var
eval env (List (Atom "define" : DottedList (Atom var : params) varargs : body)) = 
    makeVarArgs varargs env params body >>= defineVar env var
eval env (List (Atom "lambda" : List params : body)) = 
    makeNormalFunc env params body
eval env (List (Atom "lambda" : DottedList params varargs : body)) =
    makeVarArgs varargs env params body
eval env (List (Atom "lambda" : varargs@(Atom _) : body)) = 
    makeVarArgs varargs env [] body
eval env (List [Atom "set!", Atom var, val]) = eval env val >>= setVar env var
eval env (List [Atom "if", pred, conseq, alt]) = do 
                         b <- eval env pred 
                         case b of
                               Bool False -> eval env alt
                               Bool True  -> eval env conseq
                               _ -> throwError $ TypeMismatch "bool" pred

eval env (List ((Atom "cond"):cs)) = 
    do 
        b <- mapM condClause cs >>= liftT . cdr . take 1 . dropWhile f
        liftT (car [b]) >>= eval env
            where condClause (List [p,b]) = do q <- eval env p
                                               case q of
                                                 Bool _ -> return $ List [q, b]
                                                 _      -> throwError $ TypeMismatch "bool" q
                  condClause v = throwError $ TypeMismatch "(pred body)" v
                  f = \(List [p, b]) -> case p of 
                                            (Bool False) -> True
                                            _ -> False

{- Need to work on the case evaluation to not use elem so no need to derive Eq
   eval env pred@(List ((Atom "case") : k : c : cs)) = 
    if null (c : cs) then
        throwError $ BadSpecialForm "no true clause in case expression " pred
    else case c of 
            List (Atom "else" : exprs) -> eval env (last exprs)
            List ((List conds) : exprs) ->
                do res <- eval env k
                   eq <- liftT $ mapM (\x -> eqv [res, x]) conds
                   if Bool True `elem` eq then
                         eval env (last exprs)
                   else eval env $ List (Atom "case" : k : cs)
            _ -> throwError $ BadSpecialForm "bad-formed case expression " pred 
-}
eval env (List [Atom "quote", val]) = return val
eval env (List (func : args)) = do
            fn <- eval env func 
            mapM (eval env) args >>= apply fn

eval env errForm = throwError $ BadSpecialForm  "Unrecognized special form" errForm

apply :: LispVal -> [LispVal] -> IOThrowsError LispVal
apply (PrimitiveFunc func) args = liftT $ func args
apply (Func params varargs body closure) args =
    if num params /= num args && isNothing varargs then
        throwError $ NumArgs (num params) args
    else liftIO (bindVars closure $ zip params args) >>= bindVarArgs varargs >>= evalBody
        where remArgs = drop (length params) args
              num = toInteger . length
              evalBody env = last <$> mapM (eval env) body
              bindVarArgs arg env = 
                  case arg of
                    Just argName -> liftIO $ bindVars env [(argName, List remArgs)]
                    Nothing -> return env

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
              ("=", numBoolBinop (==)),
              ("<", numBoolBinop (<)),
              (">", numBoolBinop (>)),
              ("/=", numBoolBinop (/=)),
              (">=", numBoolBinop (>=)),
              ("<=", numBoolBinop (<=)),
              ("&&", boolBoolBinop (>=)),
              ("||", boolBoolBinop (<=)),
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
              ("string-length", strLen),
              ("string-ref", strRef),
              ("make-string", mkStr)]

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op args = Number . foldl1 op <$> mapM unpackNum args 

unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
unaryOp p [arg] = return $ p arg 

boolBinop :: (LispVal -> ThrowsError a) -> (a -> a -> Bool) -> [LispVal] -> ThrowsError LispVal
boolBinop unpacker op args = if length args /= 2 then
                                throwError $ NumArgs 2 args
                             else
                                do left <- unpacker $ head args
                                   right <- unpacker $ (head . tail) args
                                   return $ Bool $ left `op` right

numBoolBinop = boolBinop unpackNum
boolBoolBinop = boolBinop unpackBool
strBoolBinop = boolBinop unpackStr

unpackNum :: LispVal -> ThrowsError Integer 
unpackNum (Number n) = return n
unpackNum notNum = throwError $ TypeMismatch "number" notNum

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

-- car is basically head
car :: [LispVal] -> ThrowsError LispVal
car [List (x : xs)] = return x
car [DottedList (x : xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArg = throwError $ NumArgs 1 badArg

-- cdr is tail
cdr :: [LispVal] -> ThrowsError LispVal
cdr [List (x : xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_ : xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArg = throwError $ NumArgs  1 badArg

cons :: [LispVal] -> ThrowsError LispVal
cons [x, List xs] = return $ List (x : xs)
cons [x, DottedList xs y] = return $ DottedList (x : xs) y
cons [x, y] = return $ DottedList [x] y
cons badArg = throwError $ NumArgs  2 badArg

eqv :: [LispVal] -> ThrowsError LispVal
eqv [Bool x, Bool y] = return $ Bool (x == y)
eqv [Number x, Number y] = return $ Bool (x == y)
eqv [String x, String y] = return $ Bool (x == y)
eqv [Atom x, Atom y] = return $ Bool (x == y)
eqv zippedBoi@[List _, List _] = eqvList eqv zippedBoi 
eqv [DottedList xs x, DottedList ys y] = eqv [List (xs ++ [x]), List (ys ++ [y])]
eqv [_, _] = return $ Bool False 
eqv badArg = throwError $ NumArgs 2 badArg

-- equal? implementation
-- Uses Existential types
-- to allow usage of multiple
-- types in one List
data Unpacker = forall a . Eq a => AnyUnpacker (LispVal -> ThrowsError a)

unpackEq :: LispVal -> LispVal -> Unpacker -> ThrowsError Bool
unpackEq x y (AnyUnpacker unpacker) = 
        do unpackedx <- unpacker x
           unpackedy <- unpacker y
           return $ unpackedx == unpackedy
    `catchError` const (return False)

equal :: [LispVal] -> ThrowsError LispVal
equal zippedBoi@[List _, List _] = eqvList equal zippedBoi 
equal [DottedList xs x, DottedList ys y] = equal [List $ xs ++ [x], List $ ys ++ [y]]
equal [x, y] = do
            primitiveEqs <- or <$> mapM (unpackEq x y)
                            [AnyUnpacker unpackNum, AnyUnpacker unpackStr, AnyUnpacker unpackBool]
            eqvEquals <- eqv [x, y]
            return $ Bool (primitiveEqs || let (Bool x) = eqvEquals in x)
equal badArg = throwError $ NumArgs  2 badArg

eqvList :: ([LispVal] -> ThrowsError LispVal) -> [LispVal] -> ThrowsError LispVal
eqvList eqvFnc [List xs, List ys] = return $ Bool $ (length xs == length ys) && all checkPair (zip xs ys)
    where checkPair (x, y) = case eqvFnc [x, y] of
                               Left err -> False
                               Right (Bool val) -> val

strLen :: [LispVal] -> ThrowsError LispVal
strLen [String s] = Right $ Number $ fromIntegral $ length s
strLen [notStr] = throwError $ TypeMismatch "string" notStr
strLen badArg = throwError $ NumArgs 1 badArg

strRef :: [LispVal] -> ThrowsError LispVal
strRef [String s, Number n] =
        if length s < ind + 1 then
            throwError $ Default "Out of bound error"
        else Right $ String [s !! ind]
            where ind = fromIntegral n
strRef [String _, notNum] = throwError $ TypeMismatch "number" notNum
strRef [notString, _] = throwError $ TypeMismatch "string" notString
strRef badArg = throwError $ NumArgs 2 badArg

mkStr :: [LispVal] -> ThrowsError LispVal
mkStr [Number n, Character c] = return $ String $ replicate (fromIntegral n) c
mkStr [Number n] = return $ String $ replicate (fromIntegral n) '\x0'

makeFunc varargs env params body = return $ Func (map showVal params) varargs body env
makeNormalFunc = makeFunc Nothing
makeVarArgs = makeFunc . Just . showVal


