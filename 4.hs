{-
function cp: takes two string - <filename 1 / path> <filename 2 / path>
copy the contents of the first file and giving the copy the second file name
-}


cp :: IO ()
cp = do
    putStr "Enter name of the 1st file: "
    file1 <- getLine
    putStr "Enter name of the 2nd file: "
    file2 <- getLine
    contents <- readFile file1
    writeFile file2 contents