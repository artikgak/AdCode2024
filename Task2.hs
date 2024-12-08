module Task2 where

import System.IO
import Data.List
import Data.List.Split

main :: IO ()
main = do
    --handle <- openFile "test1.txt" ReadMode
    handle <- openFile "task2.txt" ReadMode
    contents <- hGetContents handle
    --print $ lines contents
    let listOfPairs = map splitNumbers $ lines contents
        
    print $ "Part 1: " ++ (show $ length $ filter isSafe listOfPairs)
    print $ "Part 2: " ++ (show $ length $ filter isSafeWithRemoval listOfPairs)
    hClose handle

splitNumbers :: String -> [Int]
splitNumbers str = map read splitted where 
    splitted = splitOn " " str

isSafe :: [Int] -> Bool
isSafe ns = isDiffSafe && (isMonotoneUp || isMonotoneDown) where
    pairs = zip ns (tail ns)
    isDiffSafe = all (\(a,b) -> a/=b && abs(a-b) <= 3) pairs
    isMonotoneUp = all (\(a,b) -> a<b) pairs
    isMonotoneDown = all (\(a,b) -> a>b) pairs

isSafeWithRemoval :: [Int] -> Bool
isSafeWithRemoval ns = (isSafe ns) || (any isSafe tryRemovals) where
    tryRemovals = map (\i -> take i ns ++ drop (i+1) ns) [0..(length ns - 1)]
