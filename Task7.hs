module Task7 where

import System.IO
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Tuple

data Direction = Add | Mult | Concat deriving (Show, Eq, Ord, Enum)

main :: IO ()
main = do
    --handle <- openFile "test7.txt" ReadMode
    handle <- openFile "task7.txt" ReadMode
    contents <- hGetContents handle
    let
        input = splitOn "\n" contents
        exprsRaw = map (splitOn ": ") input
        exprs = map (\xs -> ((read::String -> Int) $ head xs, map (read::String -> Int) $ splitOn " " $ last xs)) $ exprsRaw
        canBeQualizedRes = filter canEvalAnyWay exprs
        canBeQualizedRes2 = filter serchEvaluateion exprs
    --print $ exprsRaw
    --print $ exprs
    --print $ canBeQualizedRes
    print $ "Part 1: " ++ (show $ sum $ map fst canBeQualizedRes)
    print $ "Part 2: " ++ (show $ sum $ map fst canBeQualizedRes2)
    hClose handle

generateOperations :: Int -> [[Direction]]
generateOperations n = helper n [[]] where
    helper 0 res = res
    helper n res = helper (n-1) (map (Add:) res) ++ helper (n-1) (map (Mult:) res)

generateOperations' :: Int -> [[Direction]]
generateOperations' n = helper n [[]] where
    helper 0 res = res
    helper n res = helper (n-1) (map (Add:) res) ++ helper (n-1) (map (Mult:) res) ++ helper (n-1) (map (Concat:) res)

evaluate :: [Int] -> Int -> [Direction] -> Bool
evaluate (n:nums) trg ops = (helper nums ops n) == trg where
    helper [] _ res = res
    helper (x:xs) (Add:ops)    res = if res >= trg then res else helper xs ops (res + x)
    helper (x:xs) (Mult:ops)   res = if res >= trg then res else helper xs ops (res * x)
    helper (x:xs) (Concat:ops) res = if res >= trg then res else helper xs ops (read $ show res ++ show x)

serchEvaluateion :: (Int,[Int]) -> Bool
serchEvaluateion (trg, nums) = helper trg [nums] where
    helper :: Int -> [[Int]] -> Bool
    helper trg [] = False
    helper trg (e:evals) | length e == 1 && head e == trg = True
                         | length e == 1 = helper trg evals
                         | head e > trg = helper trg evals
                         | otherwise = helper trg (evaCases ++ evals) where
                            evaCases = map (:(drop 2 e)) [(e!!0 + e!!1),(e!!0 * e!!1),(read $ show (e!!0) ++ show (e!!1))]

canEvalAnyWay :: (Int,[Int]) -> Bool
canEvalAnyWay (trg,xs) = any (evaluate xs trg) (generateOperations (length xs - 1)) 