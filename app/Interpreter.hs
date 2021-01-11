import Evaluator
import LispVal
import SimpleParser
import VarEnv
import System.IO
import System.Environment

flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

readPrompt :: String -> IO String
readPrompt prompt = flushStr prompt >> getLine 

primitiveBind :: IO Env
primitiveBind = nullEnv >>= flip bindVars (map makePrimitiveFunc primitives)
    where makePrimitiveFunc (var, func) = (var, PrimitiveFunc func)

evalString :: Env -> String -> IO String
evalString env expr = runIOThrows $ fmap show $ liftT (readExpr expr) >>= eval env

evalAndPrint :: Env -> String -> IO ()
evalAndPrint env expr = evalString env expr >>= putStrLn

until_ :: Monad m => (a -> Bool) -> m a -> (a -> m ()) -> m ()
until_ pred prompt action = do
    result <- prompt
    if pred result then return ()
    else action result >> until_ pred prompt action

runRepl :: IO ()
runRepl = primitiveBind >>= until_ (`elem` [":quit", ":q"]) (readPrompt "Lisp>> ") . evalAndPrint

runOnce :: String -> IO ()
runOnce expr = primitiveBind >>= flip evalAndPrint expr

main :: IO ()
main = do args <- getArgs
          case length args of
            0 -> runRepl
            1 -> runOnce $ head args
            _ -> putStrLn "Program takes only 0 or 1 args"
