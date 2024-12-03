module Task1 where

import System.IO
import Data.List
import Data.List.Split
import Data.Char (isDigit)

main :: IO ()
main = do
    --handle <- openFile "test1.txt" ReadMode
    handle <- openFile "task3.txt" ReadMode
    contents <- hGetContents handle
    let 
        splitted = map (splitOn ",") .  concatMap (splitOn ")") . splitOn "mul("
        filtered = filter (\x -> (length x == 2) && (all isDigit $ head x) && (all isDigit $ last x))
        readAndCalc = foldl1 (+) . map (\[x,y] -> if null x || null y then 0 else (read x :: Int) * (read y :: Int))
        res1 = (readAndCalc . filtered . splitted) contents
        res2 = (readAndCalc . filtered . splitted . getEnabledParts) contents
    --print $ "Input: " ++ contents
    --print $ "splitted: " ++ (show $ splitted contents)
    --print $ "filtered: " ++ (show $ (filtered . splitted) contents)
    print $ "Part 1: " ++ (show res1)
    --print $ "EnabledParts: " ++ (getEnabledParts contents)
    print $ "Part 2: " ++ (show res2)
    hClose handle

getEnabledParts :: String -> String
getEnabledParts x = concatMap (head) $ map (splitOn "don't()") $ splitOn "do()" x