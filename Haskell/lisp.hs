module Main(to_string, main, read_from, atom) where
--module Main(..) where
import System.IO (hFlush, stdout)
import Debug.Trace
import Control.Exception
import Prelude hiding (catch)

data Expr = LispyInt Integer |
            LispyFloat Double |
            LispySymbol String |
--            [Expr]
            LispyList [Expr]

instance Show Expr where
  show (LispyInt x) = "Int:" ++ (show x)
  show (LispySymbol x) = "Str:" ++ x
  show (LispyList x) = "List:[" ++ unwords (map show x) ++ "]"


--eval :: [Expr] -> Expr
--eval :: Expr -> Expr
--eval = undefined
--eval a = LispyList a
eval a = a

atom :: String -> Expr
atom a = LispySymbol (show a)


--read_closing_ket : [String] -> [Expr]
--read_closing_ket [] = []
read_closing_ket 0 xs ys = [reverse (tail xs), ys]
read_closing_ket i xs (")":ys) = read_closing_ket (i-1) (")":xs) ys
read_closing_ket i xs ("(":ys) = read_closing_ket (i+1) ("(":xs) ys
read_closing_ket i xs (y:ys) = read_closing_ket i (y:xs) ys


--read_from :: [String] -> [Expr]
--read_from :: [String] -> Expr
read_from [] = LispyList []
read_from (")":xs) = trace (show xs) (LispyList (error "unexpected )"))
read_from ("(":xs) = let parts = read_closing_ket 1 [] xs
                         x = read_from (head parts)
                         LispyList y = read_from (last parts)
                     in LispyList (x : y)
read_from (x:xs) = let LispyList lst = read_from xs
                   in LispyList (atom x : lst)

--parse_string = undefined

tokenize :: String -> [String]
tokenize [] = []
tokenize s = words $ addSpace s
  where
    addSpace [] = []
    addSpace (x:xs) = if x == '(' || x == ')'
                      then ' ':x:' ':(addSpace xs)
                      else x:(addSpace xs)

--parse :: String -> [Expr]
--parse :: String -> Expr
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
--  putStrLn $ show (LispyInt 1)
--  putStrLn $ show (LispySymbol "hello")
--  putStrLn $ show (LispyList [LispyInt 1, LispySymbol "a"])
--  putStrLn $ show $ tokenize "(hoge 1 (add 1 2))"
--  putStrLn $ show $ read_from $ tokenize "(hoge 1 (add 1 2))"
  putStrLn $ (to_string (eval (parse "(hoge 1 (add 1 2) (* 3 5))")))


--main = repl
main = runTest
