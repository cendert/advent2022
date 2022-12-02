module Day02 where

day02 :: IO ()
day02 = do
    content <- readFile "src/day02.example.txt"
    let contentLines = lines content
    
    return ()
