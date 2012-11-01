module Main(to_string, main) where
import System.IO (hFlush, stdout)

data Expr = LispyInt Integer |
            LispySymbol String |
            LispyList [Expr]

instance Show Expr where
  show (LispyInt x) = show x
  show (LispySymbol x) = x
  show (LispyList x) = "(" ++ unwords (map show x) ++ ")"


eval :: [Expr] -> Expr
eval = undefined

read_from :: [String] -> [Expr]
read_from = undefined

tokenize :: String -> [String]
tokenize = undefined


parse :: String -> [Expr]
parse = read_from . tokenize

to_string :: Expr -> String
to_string = show

repl = do
  putStr "MyLisp> "
  hFlush stdout
  x <- getLine
  putStr "-> "
  putStrLn (to_string (eval (parse x)))
  main

runTest = do
  putStrLn $ show (LispyInt 1)
  putStrLn $ show (LispySymbol "hello")
  putStrLn $ show (LispyList [LispyInt 1, LispySymbol "a"])


--main = repl
main = runTest
