--module Main(to_string, main, read_from, atom) where
module Main(..) where
import System.IO (hFlush, stdout)
import Control.Exception
import Prelude hiding (catch)

data Expr = LispyInt Integer |
            LispyFloat Double |
            LispySymbol String |
            LispyList [Expr]

instance Show Expr where
  show (LispyInt x) = show x
  show (LispySymbol x) = x
  show (LispyList x) = "(" ++ unwords (map show x) ++ ")"


eval :: [Expr] -> Expr
eval = undefined

atom :: String -> Expr
atom = undefined

read_from :: [String] -> [Expr]
read_from [] = []
read_from (")":xs) = error "unexpected )"
read_from ("(":xs) = (atom (head xs) : (read_from (tail (tail xs)))

tokenize :: String -> [String]
tokenize [] = []
tokenize s = words $ addSpace s
  where
    addSpace [] = []
    addSpace (x:xs) = if x == '(' || x == ')'
                         then ' ':x:' ':(addSpace xs)
                         else x:(addSpace xs)

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
  putStrLn $ show $ tokenize "(hoge 1 (add 1 2))"


--main = repl
main = runTest
