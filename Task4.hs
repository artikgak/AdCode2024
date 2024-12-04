module Task1 where

import System.IO
import Data.List.Split
import Data.List

main :: IO ()
main = do
    --handle <- openFile "test4.txt" ReadMode
    handle <- openFile "task4.txt" ReadMode
    contents <- hGetContents handle
    let 
        matrix = lines contents

        rowsCount = sum $ map (countSubstrings "XMAS") matrix
        rowsReverseCount = sum $ map (countSubstrings "SAMX") matrix

        colsCount = sum $ map (countSubstrings "XMAS") (transpose matrix)
        colsReverseCount = sum $ map (countSubstrings "SAMX") (transpose matrix)

        diagonals1Count = sum $ map (countSubstrings "XMAS") (diagonals matrix)
        diagonals1ReverseCount = sum $ map (countSubstrings "SAMX") (diagonals matrix)

        diagonals2Count = sum $ map (countSubstrings "XMAS") (diagonals $ map reverse matrix)
        diagonals2ReverseCount = sum $ map (countSubstrings "SAMX") (diagonals $ map reverse matrix)

        checkXPart2 = map (isXMAS matrix) [(x,y) | x <- [1..(length matrix - 2)], y <- [1..(length (matrix !! 0) - 2)]]
        countPart2 = length $ filter (==True) checkXPart2
    --print $ "Input: " ++ (show matrix)
    print $ "Part 1: " ++ (show (rowsCount + rowsReverseCount + colsCount + colsReverseCount + diagonals1Count + diagonals1ReverseCount + diagonals2Count + diagonals2ReverseCount))
    print $ "Part 2: " ++ (show countPart2)
    hClose handle

countSubstrings :: String -> String -> Int
countSubstrings sub str = length (splitOn sub str) - 1

diagonals :: [[a]] -> [[a]]
diagonals []       = []
diagonals ([]:xss) = xss
diagonals xss      = zipWith (++) (map ((:[]) . head) xss ++ repeat [])
                                  ([]:(diagonals (map tail xss)))

isXMAS :: [String] -> (Int, Int) -> Bool
isXMAS matrix (x,y) | x <= 0 || y <= 0 || x >= length matrix - 1 || y >= length (matrix !! 0) - 1 = False
                    | otherwise = (matrix !! x !! y == 'A') && (((matrix !! (x-1) !! (y-1) == 'M') && (matrix !! (x+1) !! (y+1) == 'S')) || ((matrix !! (x-1) !! (y-1) == 'S') && (matrix !! (x+1) !! (y+1) == 'M'))) && (((matrix !! (x-1) !! (y+1) == 'M') && (matrix !! (x+1) !! (y-1) == 'S')) || ((matrix !! (x-1) !! (y+1) == 'S') && (matrix !! (x+1) !! (y-1) == 'M')))