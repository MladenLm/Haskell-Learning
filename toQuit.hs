main :: IO ()
main = do
    putStrLn "Write down what you want to say\nWrite Quit to exit the program"
    i <- getLine
    if i /= "Quit" then do
        putStrLn ("Input: " ++ i ++ ".")
        main
    else
        return ()