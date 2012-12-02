
type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "test.txt" "hello world",
      Folder "tmp"
        [ File "log1.txt" ".1...loaddddd",
          File "log2.txt" "..2..loaddddd",
          File "log3.txt" "...3.loaddddd"
         ]
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)


