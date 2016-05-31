module Main(toString, main, read_from, atom) where

import System.IO (hFlush, stdout)
import Debug.Trace
import Control.Exception
import Prelude hiding (catch)

data Expr = LispInt Int |
            LispSymbol String |
            LispProc ([Expr]->Expr) |
            LispLambda [Expr] |
            LispList [Expr]

instance Show Expr where
  show (LispInt x) = "Int:" ++ (show x)
  show (LispSymbol x) = "Str:" ++ x
  show (LispList x) = "List:[" ++ unwords (map show x) ++ "]"
  show (LispProc x) = "Proc:[]"
  show (LispLambda x) = "Lambda:[" ++ unwords (map show x) ++ "]"

instance Eq Expr where
  (LispInt a) == (LispInt b) = a == b
  (LispSymbol a) == (LispSymbol b) = a == b
  (LispList a) == (LispList b) = a == b
  (LispLambda a) == (LispLambda b) = a == b
  (LispProc a) == (LispProc b) = False

instance Show Environment where
  show (Environment lst parent) = "Env:" ++ (show lst) ++ ":" ++ (show parent)

data Environment = Environment [(String, Expr)] (Maybe Environment)

errorFewArgs = error "too few arguments"
errorManyArgs = error "too many arguments"


bool :: Bool -> Expr
bool True = lispTrue
bool _ = lispFalse

procAppend :: [Expr] -> Expr
procAppend [] = LispList []
procAppend (x:xs) = LispList ((toList x) ++ (toList (procAppend xs)))

procCar :: [Expr] -> Expr
procCar [] = errorFewArgs
procCar (x:xs) = head $ toList x

procCdr :: [Expr] -> Expr
procCdr [] = errorFewArgs
procCdr (x:xs) = LispList $ tail $ toList x

procCons :: [Expr] -> Expr
procCons [] = errorFewArgs
procCons (a:(LispList es):[]) = LispList (a:es)
procCons (a:b:[]) = LispList [a,b]
procCons _ = errorManyArgs

procLength :: [Expr] -> Expr
procLength ((LispList es):[]) = LispInt (length es)
procLength _ = undefined

procList :: [Expr] -> Expr
procList [] = LispList []
procList (x:xs) = LispList (x : (toList (procList xs)))

procIsList :: [Expr] -> Expr
procIsList (LispList x:xs) = lispTrue
procIsList _ = lispFalse

procIsSymbol :: [Expr] -> Expr
procIsSymbol (LispSymbol x:xs) = lispTrue
procIsSymbol _ = lispFalse

procIsNil :: [Expr] -> Expr
procIsNil (lispNil:xs) = lispTrue
procIsNil _ = lispFalse

procNot :: [Expr] -> Expr
procNot (lispTrue:xs) = lispFalse
--procNot (lispFalse:xs) = lispTrue
procNot _ = lispTrue


procPlus :: [Expr] -> Expr
procPlus exps = LispInt $ sum $ map toInt exps

procMinus :: [Expr] -> Expr
procMinus [] = errorFewArgs
procMinus (x:[]) = LispInt (- (toInt x))
procMinus (x:xs) = LispInt ((toInt x) - (sum $ map toInt xs))

procMultiply :: [Expr] -> Expr
procMultiply [] = LispInt 1
procMultiply (LispInt 0:xs) = LispInt 0
procMultiply (LispInt x:xs) = LispInt (x * (toInt $ procMultiply xs))


procDivide :: [Expr] -> Expr
procDivide [] = LispInt 1
procDivide (LispInt 0:xs) = LispInt 0
procDivide (LispInt x:xs) = LispInt (x `div` (toInt $ procMultiply xs))

procGreater :: [Expr] -> Expr
procGreater (LispInt x:xs) = bool $ all (\i -> x > i) (map toInt xs)

procLess :: [Expr] -> Expr
procLess (LispInt x:xs) = bool $ all (\i -> x < i) (map toInt xs)

procGreaterEqual :: [Expr] -> Expr
procGreaterEqual (LispInt x:xs) = bool $ all (\i -> x >= i) (map toInt xs)

procLessEqual :: [Expr] -> Expr
procLessEqual (LispInt x:xs) = bool $ all (\i -> x <= i) (map toInt xs)

procEqual :: [Expr] -> Expr
procEqual (LispInt x:xs) = bool $ all (\i -> x == i) (map toInt xs)


globalEnvironment = Environment [
  ("append", LispProc procAppend),
  ("car", LispProc procCar),
  ("cdr", LispProc procCdr),
  ("cons", LispProc procCons),
  ("length", LispProc procLength),
  ("list", LispProc procList),

  ("list?", LispProc procIsList),
  ("symbol?", LispProc procIsSymbol),
  ("nil?", LispProc procIsNil),
  ("not", LispProc procNot),

  ("+", LispProc procPlus),
  ("-", LispProc procMinus),
  ("*", LispProc procMultiply),
  ("/", LispProc procDivide),

  (">", LispProc procGreater),
  ("<", LispProc procLess),
  (">=", LispProc procGreaterEqual),
  ("<=", LispProc procLessEqual),
  ("=", LispProc procEqual),

  ("t",lispTrue),
  ("f",lispFalse),
  ("nil", lispNil)] Nothing


dict :: Environment -> [(String, Expr)]
dict (Environment lst parent) = lst

setDict :: Environment -> String -> Expr -> Environment
setDict env s e = case findEnv (Just env) s of
  (Just (Environment lst parent)) -> Environment ((s,e):lst) parent
  Nothing -> let Environment l p = env in Environment ((s,e):l) p

defineDict (Environment lst parent) s e = Environment ((s,e):lst) parent


searchExpr :: [(String,Expr)] -> String -> Maybe Expr
searchExpr ((s,expr):lst) key = if s == key then Just expr else (searchExpr lst key)
searchExpr [] key = Nothing

findEnv :: Maybe Environment -> String -> Maybe Environment
findEnv Nothing _ = Nothing
findEnv (Just (Environment lst parent)) s = case (searchExpr lst s) of
                                            Just expr -> Just (Environment lst parent)
                                            Nothing -> findEnv parent s


headExpr :: Expr -> Expr
headExpr (LispList (x:xs)) = x
headExpr (LispLambda (x:xs)) = x
headExpr _ = atom "headExpr error!!"

tailExpr :: Expr -> [Expr]
tailExpr (LispList (x:xs)) = xs
tailExpr (LispLambda (x:xs)) = xs
tailExpr _ = [atom "headExpr error!!"]

toList :: Expr -> [Expr]
toList (LispList lst) = lst
toList _ = undefined

toSym :: Expr -> String
toSym (LispSymbol s) = s
toSym _ = undefined



eval :: Environment -> Expr -> (Environment, Expr)
eval env (LispSymbol s) = case findEnv (Just env) s of
                              Just e -> case searchExpr (dict e) s of
                                        Just expr -> (env, expr)
                                        -- ここには来ないはず
                                        Nothing -> (env, atom "NO!")
                              Nothing -> (env, atom "no!?")
eval env (LispInt i) = (env, LispInt i)
eval env (LispList []) = (env, lispNil)
eval env (LispList (LispSymbol "quote":xs)) = (env, head xs)
eval env (LispList (LispSymbol "if":xs)) = case xs of
    (cond:a:b:_) -> if lispTrue == (snd (eval env cond)) then eval env a else eval env b
    _ -> (env, lispNil)
eval env (LispList (LispSymbol "set!":xs)) = case xs of
    ((LispSymbol key):cell:_) -> let c = snd (eval env cell) in ((setDict env key c), c)
    _ -> (env, lispNil)
eval env (LispList (LispSymbol "define":xs)) = case xs of
    ((LispSymbol key):cell:_) -> let c = snd (eval env cell) in ((defineDict env key c), c)
    _ -> (env, lispNil)
eval env (LispList (LispSymbol "lambda":xs)) = (env, LispLambda (LispSymbol "lambda":xs))
eval env lst = let  proc = snd $ eval env (headExpr lst) ;
                    exps = map (snd . (eval env)) (tailExpr lst)
              in case proc of
                (LispLambda l) -> let syms = map toSym (toList (head (tailExpr proc)))
                                 in eval (Environment (zip syms exps) (Just env)) (head (tail (tailExpr proc)))
                (LispProc p) -> (env, p exps)
                _ -> (env, atom ("undefined eval:" ++ (show proc)))

atom :: String -> Expr
atom s = case reads s of
          [(x, "")] -> LispInt x
          _ -> LispSymbol s

lispNil = LispSymbol "nil"
lispTrue = LispSymbol "t"
lispFalse = LispSymbol "f"

read_list :: [Expr] -> [String] -> ([Expr], [String])
read_list exprs (")":xs) = (reverse exprs, xs)
read_list exprs (x:xs) = let (expr, ss) = read_from (x:xs)
                          in read_list (expr:exprs) ss

read_from :: [String] -> (Expr, [String])
read_from [] = error "unexpected EOF while readling"
read_from ("(":xs) = let (list, ss) = read_list [] xs
                      in (LispList list, ss)
read_from (")":xs) = error "unexpedted )"
read_from (x:xs) = (atom x, xs)


tokenize :: String -> [String]
tokenize [] = []
tokenize s = words $ addSpace s
  where
    addSpace [] = []
    addSpace (x:xs) = if x == '(' || x == ')'
                      then ' ':x:' ':(addSpace xs)
                      else x:(addSpace xs)

parse :: String -> Expr
parse str = fst (read_from (tokenize str))

toString :: Expr -> String
toString = show

toInt :: Expr -> Int
toInt exp = case exp of
             LispInt i -> i
             _ -> error "not int"

repl env = do
  putStr "MyLisp> "
  hFlush stdout
  x <- getLine
  putStr "-> "
  let (e,expr) = eval env $ parse x
  putStrLn $ toString expr
  repl e

main = repl globalEnvironment


