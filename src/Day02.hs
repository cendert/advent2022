module Day02 where

data HandShape = Rock | Paper | Scissors deriving (Read, Show)
data PlayResult = Win | Draw | Loss deriving (Show)

processLineScore :: String -> Int
processLineScore xs = getPlayerScore (charToHandShape (last xs)) (charToHandShape (head xs))

charToHandShape :: Char -> HandShape
charToHandShape ch
  | ch == 'A' || ch == 'X' = Rock
  | ch == 'B' || ch == 'Y' = Paper
  | ch == 'C' || ch == 'Z' = Scissors

getHandTypeScore :: HandShape -> Int
getHandTypeScore Rock = 1
getHandTypeScore Paper = 2
getHandTypeScore Scissors = 3

getPlayResultScore :: PlayResult -> Int
getPlayResultScore Win = 6
getPlayResultScore Draw = 3
getPlayResultScore Loss = 0

getPlayerScore :: HandShape -> HandShape -> Int
getPlayerScore Rock Rock = getHandTypeScore Rock + getPlayResultScore Draw
getPlayerScore Rock Paper = getHandTypeScore Rock + getPlayResultScore Loss
getPlayerScore Rock Scissors = getHandTypeScore Rock + getPlayResultScore Win
getPlayerScore Paper Rock = getHandTypeScore Paper + getPlayResultScore Win
getPlayerScore Paper Paper = getHandTypeScore Paper + getPlayResultScore Draw
getPlayerScore Paper Scissors = getHandTypeScore Paper + getPlayResultScore Loss
getPlayerScore Scissors Rock = getHandTypeScore Scissors + getPlayResultScore Loss
getPlayerScore Scissors Paper = getHandTypeScore Scissors + getPlayResultScore Win
getPlayerScore Scissors Scissors = getHandTypeScore Scissors + getPlayResultScore Draw

day02 :: IO ()
day02 = do
    content <- readFile "src/day02.input.txt"
    let contentLines = lines content
    let scoresList = map processLineScore contentLines
    let finalScore = sum scoresList
    print finalScore
    
    return ()
