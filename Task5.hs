module Task5 where

import System.IO
import Data.List.Split
import Data.List
import Data.Maybe

main :: IO ()
main = do
    --handle <- openFile "test5.txt" ReadMode
    handle <- openFile "task5.txt" ReadMode
    contents <- hGetContents handle
    let
        rul_data = splitOn "\n\n" contents
        rules = parseRules (head rul_data)
        datas = parseData (last rul_data)
        filtered = filter (\d -> all (checkFollowsRule d) rules) datas
        res1 = sum $ map (\xs -> xs!!(length xs `div` 2)) filtered

        badData = filter (\d -> not $ all (checkFollowsRule d) rules) datas
        fixedData = map (fixData rules) badData 
        res2 = sum $ map (\xs -> xs!!(length xs `div` 2)) fixedData

    --print $ "rules: " ++ (show rules)
    --print $ "data: " ++ (show datas)
    --print $ "filtered: " ++ (show filtered)
    print $ "Part 1: " ++ (show res1)
    print $ "badData: " ++ (show badData)
    print $ "fixedData: " ++ (show fixedData)
    print $ "Part 2: " ++ (show res2)
    hClose handle

parseRules :: String -> [(Int, Int)]
parseRules str = map (\x -> (read (head x), read (last x))) $ map (splitOn "|") $ splitOn "\n" str

parseData :: String -> [[Int]]
parseData str = map (\xs -> map (read :: String -> Int) xs) $ map (splitOn ",") $ splitOn "\n" str

checkFollowsRule :: [Int] -> (Int, Int) -> Bool
checkFollowsRule xs (a,b) = case (elemIndex a xs, elemIndex b xs) of
    (Just i, Just j) -> i < j
    _ -> True

fixData :: [(Int, Int)] -> [Int] -> [Int]
fixData rules xs = untilStable (goThroughtRules rules) xs

goThroughtRules :: [(Int, Int)] -> [Int] -> [Int]
goThroughtRules []          xs = xs
goThroughtRules ((a,b):rules) xs  | checkFollowsRule xs (a,b) = goThroughtRules rules xs
                                | otherwise = goThroughtRules rules (workOneRule xs (a,b))

workOneRule :: [Int] -> (Int, Int) -> [Int]
workOneRule xs (a,b) = res where
    i = fromJust $ elemIndex a xs
    j = fromJust $ elemIndex b xs
    insertedPrev = insertAt b i xs
    res = if i > j then insertAt b (fromJust $ elemIndex a xs) (delete b xs) else xs

untilStable :: Eq a => (a -> a) -> a -> a
untilStable f a = until (\w -> f w == w) f a

insertAt :: a -> Int -> [a] -> [a]
insertAt newElement 0 as = newElement:as
insertAt newElement i (a:as) = a : insertAt newElement (i - 1) as