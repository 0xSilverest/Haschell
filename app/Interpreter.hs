import Evaluator
import LispVal
import SimpleParser
import System.Environment

main :: IO ()
main = getArgs >>= print . eval . readExpr . head 
