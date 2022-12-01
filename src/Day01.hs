module Day01 where

day01 :: IO ()
day01 = do
    content <- readFile "src/day01.input.txt"
    let contentLines = lines content
    print contentLines
    return ()


