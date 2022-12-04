module Day03 where

day03 :: IO ()
day03 = do
    content <- readFile "src/day03.example.txt"
    -- content <- readFile "src/day03.input.txt"
    let contentLines = lines content
    
    print contentLines
    
    putStrLn "String"
    return ()
