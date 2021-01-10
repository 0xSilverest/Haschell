module VarEnv where

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad.Except (ExceptT, runExceptT, throwError, liftIO)
import Data.Maybe (isJust)

import LispVal
import LispError

type Env = IORef [(String, IORef LispVal)]
type IOThrowsError = ExceptT LispError IO

nullEnv :: IO Env
nullEnv = newIORef []

liftT :: ThrowsError a -> IOThrowsError a
liftT expr = case expr of 
               Left err -> throwError err
               Right val -> return val

runIOThrows :: IOThrowsError String -> IO String
runIOThrows act = extractValue <$> runExceptT (trapError  act)

isBound :: Env -> String -> IO Bool
isBound envRef var = isJust . lookup var <$> readIORef envRef

getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var = do  env <- liftIO $ readIORef envRef
                        maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                              (liftIO . readIORef)
                              (lookup var env)
                    
setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var val = do  env <- liftIO $ readIORef envRef
                            maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                  (liftIO . flip writeIORef val)
                                  (lookup var env)
                            return val

defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var val =  
    do isDef <- liftIO $ isBound envRef var
       if isDef 
            then setVar envRef var val >> return val
            else liftIO $ 
                    do valueRef <- newIORef val
                       env <- readIORef envRef
                       writeIORef envRef ((var, valueRef) : env)
                       return val

bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
                where extendEnv bidings env = fmap (++ env) (mapM addBinding bindings)
                      addBinding (var, val) = do ref <- newIORef val 
                                                 return (var, ref)


