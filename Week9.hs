


greeting :: IO ()
greeting = do
    putStrLn "What is your name?"
    str <- getLine
    putStrLn $ "Hello, " ++ str

addTwoNumbers :: IO ()
addTwoNumbers = do
    putStrLn "Enter a number: "
    n <- getLine
    let a = (read n :: Int)
    putStrLn "Enter another number: "
    n2 <- getLine
    let b = (read n :: Int)
    print (a + b)

copyFile :: IO ()
copyFile = do
    putStrLn "Name of file to copy: "
    oldName <- getLine
    putStrLn "Name of new file: "
    newName <- getLine
    content <- readFile oldName
    writeFile newName content
    
