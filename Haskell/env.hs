

--data Env = { dict :: [(String,String)], parent :: Integer}
-- data Env = Env { dict :: [(String, (a -> b))], parent :: Maybe Env}
--
--data Sexp = Nil | Symbol String | Subr (Sexp->Sexp) 

data MyLst = Nil | Symbol String | SymbolInt Int | Cons MyLst MyLst deriving (Show)
