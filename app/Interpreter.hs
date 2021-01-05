import LispVal
import SimpleParser
import System.Environment

main :: IO ()
main = do
        (expr : _) <- getArgs
        putStrLn $ readExpr expr


