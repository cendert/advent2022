module Day02 where

data HandShape = Rock | Paper | Scissors deriving (Read, Show)
data PlayResult = Win | Draw | Loss deriving (Show)

getHandTypeScore :: HandShape -> Int
getHandTypeScore Rock = 1
getHandTypeScore Paper = 2
getHandTypeScore Scissors = 3

getPlayResultScore :: PlayResult -> Int
getPlayResultScore Win = 6
getPlayResultScore Draw = 3
getPlayResultScore Loss = 0

getPlayer1Score :: HandShape -> HandShape -> Int
getPlayer1Score Rock Rock = getHandTypeScore Rock + getPlayResultScore Draw
getPlayer1Score Rock Paper = getHandTypeScore Rock + getPlayResultScore Win
getPlayer1Score Rock Scissors = getHandTypeScore Rock + getPlayResultScore Loss
getPlayer1Score Paper Rock = getHandTypeScore Paper + getPlayResultScore Win
getPlayer1Score Paper Paper = getHandTypeScore Paper + getPlayResultScore Draw
getPlayer1Score Paper Scissors = getHandTypeScore Paper + getPlayResultScore Loss
getPlayer1Score Scissors Rock = getHandTypeScore Scissors + getPlayResultScore Loss
getPlayer1Score Scissors Paper = getHandTypeScore Scissors + getPlayResultScore Win
getPlayer1Score Scissors Scissors = getHandTypeScore Scissors + getPlayResultScore Draw

day02 :: IO ()
day02 = do
    content <- readFile "src/day02.example.txt"
    let contentLines = lines content
    print contentLines
    let temp = read "Rock" :: HandShape
    print temp
    
    return ()
