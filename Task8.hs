module Task8 where

import System.IO
import Data.List.Split
import Data.List
import Data.Maybe
import Data.Tuple

main :: IO ()
main = do
    --handle <- openFile "test8.txt" ReadMode
    handle <- openFile "task8.txt" ReadMode
    contents <- hGetContents handle
    
    let
        matrix = splitOn "\n" contents
        cellIndices = [(i, j) | i <- [0..(length matrix - 1)], j <- [0..(length (matrix !! 0) - 1)]]
        withCoords = zip (concat matrix) cellIndices
        anthenas = sort $ filter (\x -> fst x /= '.') withCoords
        anthSplitted = splitIntoAnthenaTypes anthenas
        antinodesRaw = concatMap (findAntinodes . snd) anthSplitted
        antinodesFiltered = nub $ filter (\(x,y) -> 0 <= x && x < length matrix && 0 <= y && y < length (matrix !! 0)) antinodesRaw


        antinodesRaw2 = concatMap ((findAntinodes2 (length matrix, length (matrix !! 0))) . snd) anthSplitted
        antinodesFiltered2 = nub $ filter (\(x,y) -> 0 <= x && x < length matrix && 0 <= y && y < length (matrix !! 0)) antinodesRaw2
    --print $ matrix
    --print $ withCoords
    print $ anthenas
    print $ anthSplitted
    print $ antinodesFiltered
    print $ "Part 1: " ++ (show $ length antinodesFiltered)
    --print $ antinodesRaw2
    --print $ antinodesFiltered2
    print $ "Part 2: " ++ (show $ length antinodesFiltered2)
    hClose handle

splitIntoAnthenaTypes :: [(Char, (Int, Int))] -> [(Char, [(Int, Int)])]
splitIntoAnthenaTypes xs = helper xs [] where
    helper :: [(Char, (Int, Int))] -> [(Char, [(Int, Int)])] -> [(Char, [(Int, Int)])]
    helper [] res = res
    helper (x:xs) [] = helper xs [(fst x, [snd x])]
    helper (x:xs) res@((ch, chs):rs)    | fst x == ch = helper xs ((ch, (snd x):chs):rs)
                                    | otherwise = helper xs ((fst x, [snd x]):res)
    
findAntinodes :: [((Int, Int))] -> [(Int, Int)]
findAntinodes xs = res where
    pairs = [(x, y) | x <- xs, y <- xs, x /= y]
    res = nub $ concatMap makeTwoAntinodes pairs

findAntinodes2 :: (Int,Int) ->  [(Int, Int)] -> [(Int, Int)]
findAntinodes2 (rows,cols) xs = res where
    pairs = [(x, y) | x <- xs, y <- xs, x /= y]
    res = nub $ concatMap (makeAntinodes (rows,cols)) pairs

makeTwoAntinodes :: ((Int, Int), (Int, Int)) -> [(Int, Int)]
makeTwoAntinodes ((a,b), (x,y)) = [p1,p2] where
    (dX, dY) = (a-x, b-y)
    p1 = (a+dX, b+dY)
    (dX2, dY2) = (x-a, y-b)
    p2 = (x+dX2, y+dY2)

makeAntinodes :: (Int,Int) -> ((Int, Int), (Int, Int)) -> [(Int, Int)]
makeAntinodes (rows, cols) ((a,b), (x,y)) = p1s ++ p2s where
    isInMatrix :: (Int,Int) -> Bool
    isInMatrix (x,y) = 0 <= x && x < rows && 0 <= y && y < cols

    helper :: (Int,Int) -> (Int,Int) -> [(Int, Int)] -> [(Int, Int)]
    helper (x,y) (dx,dy) res | isInMatrix (x+dx,y+dy) = helper (x+dx,y+dy) (dx,dy) ((x+dx,y+dy):res)
                             | otherwise = res

    (dX, dY) = (a-x, b-y)
    p1s = helper (a, b) (dX, dY) [(a, b)]
    (dX2, dY2) = (x-a, y-b)
    p2s = helper (x, y) (dX2, dY2) [(x, y)]