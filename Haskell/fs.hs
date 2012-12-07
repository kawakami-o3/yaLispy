import Data.List (break)

x -: f = f x


type Name = String
type Data = String
data FSItem = File Name Data | Folder Name [FSItem] deriving (Show)

myDisk :: FSItem
myDisk =
  Folder "root"
    [ File "test.txt" "hello world",
      Folder "pics"
        [
          File "etc.png" "mokhioijjd",
          File "flower.png" "flower"
        ],
      Folder "tmp"
        [ File "log1.txt" ".1...loaddddd",
          File "log2.txt" "..2..loaddddd",
          File "log3.txt" "...3.loaddddd"
         ],
      File "first.txt" "1st",
      File "second.txt" "2nd",
      File "third.txt" "3rd",
      File "forth.txt" "4th"
    ]

data FSCrumb = FSCrumb Name [FSItem] [FSItem] deriving (Show)
type FSZipper = (FSItem, [FSCrumb])

fsUp :: FSZipper -> FSZipper
fsUp (item, FSCrumb name ls rs:bs) = (Folder name (ls ++ [item] ++ rs), bs)

fsTo :: Name -> FSZipper -> FSZipper
fsTo name (Folder folderName items, bs) =
  let (ls, item:rs) = break (nameIs name) items
  in (item, FSCrumb folderName ls rs:bs)

nameIs :: Name -> FSItem -> Bool
nameIs name (Folder folderName _) = name == folderName
nameIs name (File fileName _) = name == fileName


newFocus = (myDisk, []) -: fsTo "pics" -: fsTo "etc.png"

