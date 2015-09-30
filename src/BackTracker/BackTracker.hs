{-# LANGUAGE FlexibleInstances #-}
module BackTracker.BackTracker where
import qualified Data.Matrix as M
import Data.Set 
import qualified Data.Foldable as F
import Control.Applicative 
import Prelude hiding (filter,null,map)
import Types
import Util
import System.Console.ANSI
import Control.Parallel.Strategies
import qualified Data.Vector as V
import System.TimeIt
import Control.DeepSeq

type CellMatrix = M.Matrix CellType

instance Ord (M.Matrix CellType) -- | Needed to make a set of CellMatrixes


-- | The universal set, represents all the possible values in a set
universal = fromList [1..9]

-- | Converts any foldable to a set (most often a vector to a set)
asSet:: (F.Foldable f, Ord a) => f a -> Set a
asSet = fromList . F.toList

-- | Get the row at r as a set
rowSet:: Int -> CellMatrix  -> Set CellType
rowSet r m = asSet $ M.getRow r m

-- | Get the coloumn at c as a set
colSet:: Int -> CellMatrix -> Set CellType
colSet c m = asSet $ M.getCol c m

-- | Get the square that point p is a part of as a set
squareSet:: (Int,Int) -> CellMatrix -> Set CellType
squareSet p m = asSet $ getSubmatrix p m

-- | Remove the values that haven't been guessed yet, they won't count against what's available
removeNone:: Set CellType -> Set CellType
removeNone set = filter (/=None) set

-- | The main evaluation function. Given a Matrix and a list of remaining coordinates to solve, possibly produce a solved matrix
evaluate:: M.Matrix CellType -> [(Int,Int)] -> Maybe (M.Matrix CellType)
evaluate m [] = if isSolved m then Just m else Nothing --If there aren't any coordinates left, then return the matrix if it's solved
evaluate m (cord@(x,y):cords)  --Break the list of coordinates into the head, (which is broken into x and y) and the tail
  | isKnown $ m M.! cord = evaluate m cords -- If the cell of the head coordinate is known, then evaluate the rest of the coordinates and do nothing to this one
  | (null available) = Nothing -- If there's no possible next states, we've gone down a wrong path
  | otherwise = foldl1 (<|>) $ parMap rdeepseq (\i -> evaluate (M.setElem (Unknown i) cord m) cords ) $toList available --return the available, try to solve with the current cell being replaced by a value in available. then take the first resulting matrix which produces an actually solved matrxi. If none of them produce an actually solved matrix then return nothing
                where prepareSet = map value . removeNone -- Remove the none values and turn the knowns and unkowns into ints
                      row= prepareSet $ rowSet x m --Get the row as a set and prepare it for set manipluation
                      col = prepareSet $ colSet y m --Get the column as a set and prepare it for set manipulation
                      square = prepareSet $ squareSet cord m -- Get the suqare as a set and prepare it for set maniplulation
                      available = (universal \\ row) `intersection` (universal \\ col) `intersection` (universal \\ square) --Take the different of the universal set and a generated set then take the intersection of all the resulting difference sets, produces a set of values which are not in any of the sets but are in the universal set


-- | Given a string of properly formatted input, try to evaluate it for all coordinates
runBackTracker inputData = evaluate (strToSudoku inputData) getAllCoordinates 

runBackTrackerCords:: [(Int,Int,Int)] -> Maybe (M.Matrix CellType)
runBackTrackerCords inputData=  evaluate starting getAllCoordinates 
  where
    starting::M.Matrix CellType
    starting = fillInKnowns inputData

-- | Same as evaluate, but prints out data as it runs
evaluateIO::M.Matrix CellType -> [(Int,Int)]  -> IO (Maybe (M.Matrix CellType))
evaluateIO m [] = if isSolved m then print ("Solved: " ++ prettyPrint m) >> (return $ Just m) else print "UNSOLVABLE">> (return $ Nothing)
evaluateIO m (cord@(x,y):cords) 
 | (isKnown $ m M.! cord) = evaluateIO m cords
  | (null available) = do
      print "Nothing left!"
      printStats
      return $ Nothing
 | otherwise = do
      printStats
      newStates <- mapM (\i -> evaluateIO (M.setElem (Unknown i) cord m) cords ) $ toList available
      return $ foldl1 (<|>) newStates
                where prepareSet = map value . removeNone
                      rowAvailable = universal \\ (prepareSet $ rowSet x m)
                      colAvailable = universal \\ (prepareSet $ colSet y m)
                      squareAvailable = universal \\ (prepareSet $ squareSet cord m)
                      available = rowAvailable `intersection` colAvailable `intersection` squareAvailable
                      printStats = do
                              print "Current Matrix: "
                              print m
                              print "Current Position: "
                              print cord
                              print("Row Available: ")
                              print rowAvailable
                              print "Column Available: "
                              print colAvailable
                              print "Square Available: "
                              print squareAvailable
                              print ("Available: ")
                              print available
                              putStrLn "\n\n\n"

 
runBackTrackerPeak inputData = evaluateIO starting getAllCoordinates
                               where starting = fillInKnowns inputData

analyzeSolver:: Int -> String -> IO (Double, [Maybe (M.Matrix CellType)])
analyzeSolver n input = timeItT $ mapM(\_-> return $ force $ runBackTracker input) [1..n]
