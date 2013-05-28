data EnvType = EmptyEnv | Env EnvType [(String, Node)] deriving (Show)
data Node = Node String deriving (Show)
data Tree = EmptyTree | Leaf Node | Tree Node [Tree] deriving (Show)

tokenize ::  String -> [String]
tokenize = words . addSpaceRec
  where
    addSpaceRec "" = ""
    addSpaceRec (c:t) = (addSpace c) ++ (addSpaceRec t)
    addSpace c = if c `elem` ['(', ')'] then (" "++(c:[])++" ") else (c:[])

parseTokens :: [String] -> (Tree, [String])
parseTokens ("(":m) = (Tree (Node (head m)) trees, rest)
  where
    (trees, rest) = parseRec [] (tail m)

    parseRec :: [Tree] -> [String] -> ([Tree], [String])
    parseRec acc (arr@("(":t)) = parseRec (childTree : acc) childRest
      where
        (childTree, childRest) = parseTokens arr
    parseRec acc (")":t) = (reverse acc, t)
    parseRec acc (h:t)   = parseRec ((Leaf (Node h)):acc) t
parseTokens ("Empty":t) = (EmptyTree, t)

source = "(+ 1 (* 2 3) 98)"

main = do
  putStrLn $ show $ tokenize source
  putStrLn $ show $ parseTokens $ tokenize source
