module Task6 where

import System.IO
import Data.List.Split
import Data.List
import Data.Maybe

data Direction = Undefined | U | D | L | R  deriving (Show, Eq, Ord, Enum)

main :: IO ()
main = do
    --handle <- openFile "test6.txt" ReadMode
    handle <- openFile "task6.txt" ReadMode
    contents <- hGetContents handle
    let
        matrix = splitOn "\n" contents
        loc_dir = getStartLocationAndDirection matrix
        resMatrix = goThroughMatrix matrix
        calcCells = length $ filter (\x -> x == 'X') $ concat resMatrix

        cellIndices = [(i, j) | i <- [0..(length resMatrix - 1)], j <- [0..(length (resMatrix !! 0) - 1)]]
        possibleIndicesForNewObstacle = map fst $ filter (('X'==) . snd) $ zip cellIndices (concat resMatrix)
        possibleMatrices = map (\(i, j) -> modifyElement matrix (i, j) '#') (possibleIndicesForNewObstacle \\ [(fst loc_dir)])
        resMatrices = filter hasCycle possibleMatrices
    --print $ matrix
    --print $ loc_dir
    --mapM_ print resMatrix
    print $ "Part 1: " ++ (show calcCells)
    --mapM_ print possibleMatrices
    --mapM_ print resMatrices 
    print $ "Part 2: " ++ (show $ length resMatrices)
    hClose handle

hasCycle :: [String] -> Bool
hasCycle matrix = helper (matrix,[]) (getStartLocationAndDirection matrix)

helper :: ([String],[((Int, Int), Direction)]) -> ((Int, Int), Direction) -> Bool
helper (matrix, prevStates) currstate@((row, col), dir)  | (not $ cellInBounds matrix (row, col)) = False
                                                           | elem loc_dir_till prevStates = True
                                                           | otherwise = helper (matrix, newStates) new_dir_loc
        where
            loc_dir_till = makeMoves (matrix, currstate)
            new_dir_loc = goCell loc_dir_till '#' 
            newStates = loc_dir_till:prevStates
--1482
makeMoves :: ([String], ((Int, Int), Direction)) -> ((Int, Int), Direction)
makeMoves (matrix, ((row, col), dir)) = ((nX, nY), nDir) where
    ((nX, nY), nDir) = goTillBlock matrix ((row, col), dir)

goTillBlock :: [String] -> ((Int, Int), Direction) -> ((Int, Int), Direction)
goTillBlock matrix ((row, col), dir) | (not $ cellInBounds matrix (row, col)) = ((row, col), dir)
                                      | otherwise = case dir of
                                            U -> if getElem matrix (row-1, col) == '#' then ((row, col), dir) else goTillBlock matrix ((row-1, col), dir)
                                            D -> if getElem matrix (row+1, col) == '#' then ((row, col), dir) else goTillBlock matrix ((row+1, col), dir)
                                            L -> if getElem matrix (row, col-1) == '#' then ((row, col), dir) else goTillBlock matrix ((row, col-1), dir)
                                            R -> if getElem matrix (row, col+1) == '#' then ((row, col), dir) else goTillBlock matrix ((row, col+1), dir)

needToRotate :: [String] -> ((Int, Int), Direction) -> Bool
needToRotate matrix ((row, col), dir) | (not $ cellInBounds matrix (row, col)) = False
                                       | otherwise = case dir of
                                            U -> getElem matrix (row, col-1) == '#'
                                            D -> getElem matrix (row, col+1) == '#'
                                            L -> getElem matrix (row, col-1) == '#'
                                            R -> getElem matrix (row, col+1) == '#'

goThroughMatrix :: [String] -> [String]
goThroughMatrix matrix = helper matrix (getStartLocationAndDirection matrix) where
    helper :: [String] -> ((Int, Int), Direction) -> [String]
    helper matrix ((row, col), dir)  | cellInBounds matrix (row, col) = helper new_matrix new_loc_dir
                                   | otherwise = matrix
        where
            (new_matrix, new_loc_dir) = makeMove (matrix, ((row, col), dir))

getElem :: [String] -> (Int, Int) -> Char
getElem matrix (row, col) | cellInBounds matrix (row, col) = (matrix !! row) !! col
                            | otherwise = ' '

modifyElement :: [String] -> (Int, Int) -> Char -> [String]
modifyElement matrix (row, col) elem = take row matrix ++ [take col (matrix !! row) ++ [elem] ++ drop (col + 1) (matrix !! row)] ++ drop (row + 1) matrix

cellInBounds :: [String] -> (Int, Int) -> Bool
cellInBounds matrix (row, col) = 0 <= row && row < length matrix && 0 <= col && col < length (matrix !! 0)

makeMove :: ([String], ((Int, Int), Direction)) -> ([String], ((Int, Int), Direction))
makeMove (matrix, ((row, col), dir)) = (new_mat, ((nX, nY), nDir)) where
    new_mat = modifyElement matrix (row, col) 'X'
    next_cell = case dir of 
        U -> getElem matrix (row-1, col)
        D -> getElem matrix (row+1, col)
        L -> getElem matrix (row, col-1)
        R -> getElem matrix (row, col+1)
    ((nX, nY), nDir) = goCell ((row, col), dir) next_cell

goCell :: ((Int,Int), Direction) -> Char -> ((Int, Int), Direction)
goCell ((row, col), dir) nextCell = case (dir,nextCell) of
    (U, '#') -> ((row, col), R)
    (D, '#') -> ((row, col), L)
    (L, '#') -> ((row, col), U)
    (R, '#') -> ((row, col), D)
    (U, _) -> ((row-1, col), dir)
    (D, _) -> ((row+1, col), dir)
    (L, _) -> ((row, col-1), dir)
    (R, _) -> ((row, col+1), dir)
    _        -> error "Invalid input"

getStartLocationAndDirection :: [String] -> ((Int, Int), Direction)
getStartLocationAndDirection xs = helper xs 0 where
    helper [] _ = ((-1, -1), Undefined)
    helper (x:xs) row = case elemIndex '^' x of
        Just col -> ((row, col), U)
        Nothing -> helper xs (row + 1)