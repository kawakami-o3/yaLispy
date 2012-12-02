data Expr = LispInt Integer | LispSymbol String deriving (Show)


parse :: String -> Expr
parse s = case reads s of
            [(x,"")] -> LispInt x
            _ -> LispSymbol s


