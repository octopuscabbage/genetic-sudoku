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
import System.TimeIt

type CellMatrix = M.Matrix CellType

instance Ord (M.Matrix CellType)

universal:: Set Int
universal = fromList [1,2,3,4,5,6,7,8,9]

asSet:: (F.Foldable f, Ord a) => f a -> Set a
asSet = fromList . F.toList

rowSet r m = asSet $ M.getRow r m

colSet c m = asSet $ M.getCol c m

squareSet p m = asSet $ getSubmatrix p m

removeDummy:: Set CellType -> Set CellType
removeDummy set = filter (/=None) set

evaluate:: M.Matrix CellType -> [(Int,Int)] -> Maybe (M.Matrix CellType)
evaluate m [] = if isSolved m then Just m else Nothing
evaluate m (cord@(x,y):cords) 
  | isKnown $ m M.! cord = evaluate m cords
  | (null available) = Nothing
  | otherwise = foldl1 (<|>) $ parMap rdeepseq (\i -> evaluate (M.setElem (Unknown i) cord m) cords ) $toList available
                where prepareSet = map value . removeDummy
                      row= prepareSet $ rowSet x m
                      col = prepareSet $ colSet y m
                      square = prepareSet $ removeDummy $ squareSet cord m
                      available = (universal \\ row) `intersection` (universal \\ col) `intersection` (universal \\ square)

runBackTracker inputData = evaluate (strToSudoku inputData) getAllCoordinates

runBackTrackerCords:: [(Int,Int,Int)] -> Maybe (M.Matrix CellType)
runBackTrackerCords inputData=  evaluate starting getAllCoordinates 
  where
    starting::M.Matrix CellType
    starting = fillInKnowns inputData


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
                where prepareSet = map value . removeDummy
                      rowAvailable = universal \\ (prepareSet $ rowSet x m)
                      colAvailable = universal \\ (prepareSet $ colSet y m)
                      squareAvailable = universal \\ (prepareSet $ removeDummy $ squareSet cord m)
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

analyze n input = timeIt $ mapM(\_-> print $ runBackTracker input) [1..n]
