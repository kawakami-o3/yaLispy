data Expr = LispInt Integer | LispFloat Float | LispSymbol String deriving (Show)


parse :: String -> Expr
parse s = case reads s of
            [(x,"")] -> LispInt x
            _ -> case reads s of
              [(y,"")] -> LispFloat y
              _ -> LispSymbol s

main = do
  putStrLn "hello"
  putStrLn $ show $ parse "hello"
  putStrLn $ show $ parse "1"
  putStrLn $ show $ parse "1.0"


