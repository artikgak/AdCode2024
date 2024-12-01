module Task1 where

import System.IO
import Data.List
import Data.List.Split

main :: IO ()
main = do
    --handle <- openFile "test1.txt" ReadMode
    handle <- openFile "task1.txt" ReadMode
    contents <- hGetContents handle
    --print $ lines contents
    let listOfPairs = transformLists $ map splitNumbers $ lines contents
        sorted = (\(a,b) -> (sort a, sort b))  listOfPairs
        differences = uncurry (zipWith (\x y -> abs(x-y))) sorted

        occurences = map (numberOfOccurences (snd sorted)) (fst sorted)
        
    print $ "Part 1: " ++ (show $ sum differences)
    print $ "Part 2: " ++ (show $ sum $ zipWith (*) occurences (fst sorted))
    hClose handle

splitNumbers :: String -> (Int, Int)
splitNumbers str = (read $ head splitted, read $ last splitted) where 
    splitted = splitOn "   " str

transformLists :: [(Int, Int)] -> ([Int], [Int])
transformLists xs = (map fst xs, map snd xs)

numberOfOccurences :: [Int] -> Int -> Int
numberOfOccurences xs x = length $ filter (==x) xs