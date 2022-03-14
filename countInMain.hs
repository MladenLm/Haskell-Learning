count n m = do
    putStrLn (show n)
    if n < m then
        count (n + 1) m
    else return ()