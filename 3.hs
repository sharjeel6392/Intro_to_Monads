
average :: (Fractional a) => [a] -> a
average []   = 0
average nums = let (sum,count) = foldr (\e (s,c) -> (s+e,c+1)) (0,0)  nums in sum / count


readDoubles :: String -> String -> IO [Double]
readDoubles prompt sentinel = do
    putStr prompt
    n <- getLine
    if n == sentinel
        then return []
    else do
        let digit = read n
        m <- readDoubles prompt sentinel
        return (digit : m)

interface :: IO ()
interface = do
    putStrLn "Enter some numbers."
    putStrLn "When finished, type 'done'."
    y <- readDoubles "Enter a number: " "done"
    putStrLn ("The average is " ++ show (average y))
    putStrLn ("The maximum is " ++ show (maximum y))
    putStrLn ("The minimum is " ++ show (minimum y))