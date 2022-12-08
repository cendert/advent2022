module Day05 where

day05 :: IO ()
day05 = do
    content <- readFile "src/day05.example.txt"
    -- content <- readFile "src/day05.input.txt"
    let contentLines = lines content

    return ()
