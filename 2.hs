meetAndGreet :: IO ()
meetAndGreet = 
     do
        putStrLn "Greetings once again.  What is your name?"
        inpStr <- getLine
        putStrLn ("Hello " ++ inpStr ++ "!")