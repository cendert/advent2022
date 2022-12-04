module Day02 where
import Text.Printf

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

processLineScore' :: String -> Int
processLineScore' xs = getPlayerScore' (charToHandShape (head xs)) (charToPlayResult (last xs))

charToPlayResult :: Char -> PlayResult
charToPlayResult ch
  | ch == 'X' = Loss
  | ch == 'Y' = Draw
  | ch == 'Z' = Win

getPlayerScore' :: HandShape -> PlayResult -> Int
getPlayerScore' Rock Win = getHandTypeScore Paper + getPlayResultScore Win
getPlayerScore' Rock Draw = getHandTypeScore Rock + getPlayResultScore Draw
getPlayerScore' Rock Loss = getHandTypeScore Scissors + getPlayResultScore Loss
getPlayerScore' Paper Win = getHandTypeScore Scissors + getPlayResultScore Win
getPlayerScore' Paper Draw = getHandTypeScore Paper + getPlayResultScore Draw
getPlayerScore' Paper Loss = getHandTypeScore Rock + getPlayResultScore Loss
getPlayerScore' Scissors Win = getHandTypeScore Rock + getPlayResultScore Win
getPlayerScore' Scissors Draw = getHandTypeScore Scissors + getPlayResultScore Draw
getPlayerScore' Scissors Loss = getHandTypeScore Paper + getPlayResultScore Loss

day02 :: IO ()
day02 = do
    content <- readFile "src/day02.input.txt"
    let contentLines = lines content
    let scoresList = map processLineScore contentLines
    let finalScore = sum scoresList
    printf "Final score with wrong interpretation: %i\n" finalScore
    let scoresList' = map processLineScore' contentLines
    let finalScore' = sum scoresList'
    printf "Final score with correct interpretation: %i\n" finalScore'
    
    return ()
