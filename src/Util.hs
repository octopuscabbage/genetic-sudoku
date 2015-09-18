module Util where

import Data.Matrix
import AI.GeneticAlgorithm.Simple
import System.Random 
import Data.List (foldl')
import Types
import qualified Data.Vector as V
import qualified Control.Monad.Random as R
import Data.Char

sudokuSize = 9
sudokuFromList = fromList sudokuSize sudokuSize

isSolved m = all (\p -> 0 == thingsWrongWithCell m p) getAllCoordinates && (all (\x -> value x /= 0) $ toList m)

thingsWrong:: Matrix CellType -> Int
thingsWrong m =  sum $ map (thingsWrongWithCell m) getAllCoordinates

getAllCoordinates:: [(Int, Int)]
getAllCoordinates = [(x,y) | x <- [1..sudokuSize], y <- [1..sudokuSize]]

thingsWrongWithCell:: Matrix CellType -> (Int,Int) -> Int
thingsWrongWithCell m p@(x,y) =  length $filter (==False) $ [1 ==inRowNTimes cellAt y m, 1 == inColNTimes cellAt x m,1 == inSquareNTimes p cellAt m]
                          where cellAt = m ! p
value (Known a) = a
value (Unknown a) = a
value _ = 0

inListNTimes cell l =  (length $ filter (\a -> value a== value cell) l) 

inRowNTimes cell row m = inListNTimes cell $ V.toList $  getRow row m

inColNTimes cell col m = inListNTimes cell $ V.toList $ getCol col m


inSquareNTimes p@(x,y) cell m = inListNTimes cell $ toList $ getSubmatrix p m

getSubmatrix::(Int, Int) -> Matrix CellType -> Matrix CellType
getSubmatrix (x,y) m
  | (x <= 3 && y <= 3 ) = submatrix 1 3 1 3 m
  | (x > 3 && x <= 6 && y <= 3) = submatrix 4 6 1 3 m
  | (x > 6 && y <= 3) =  submatrix 7 9 1 3  m
  | (x <= 3 && y > 3 && y <= 6) = submatrix 1 3 4 6 m
  | (x >3 && x<= 6 && y >3 && y <= 6) = submatrix 4 6 4 6 m
  | (x >6 && y > 3 && y <= 6) = submatrix 7 9 4 6 m
  | (x <= 3 && y >6) = submatrix 1 3 7 9 m
  | (x >3 && x <= 6 && y > 6) = submatrix 4 6 7 9 m
  | (x > 6 && y > 6) = submatrix 7 9 7 9 m
  | otherwise = error $ "Not found in Matrix " ++ show x ++ " "++ show y



isKnown:: CellType -> Bool
isKnown (Known _) = True
isKnown _ = False

prettyPrint:: Matrix CellType -> String
prettyPrint m = concatMap produceString $ zip [1..] $ toList m
     where lineChars = "\n" ++ map (const '-') [1..21]  ++ "\n"
           a `mods` b = a `mod` b == 0
           produceString (i,boxedVal)
             | (i `mods` 81) = show val
             | (i `mods` 27) = show val ++ lineChars
             | (i `mods` 9) = show val ++ "\n"
             | (i `mods` 3) = show val ++ " | "
             | otherwise = show val ++ " "
                           where val = value boxedVal
 
--Given a puzzle with the knowns filled in, use the random iterator g and generate a bunch of randomly filled startind ata
generateStarting:: RandomGen g => Matrix CellType -> R.Rand g (Matrix CellType)
generateStarting puzzleWithKnownsFilledIn = R.getRandomRs (1,9) >>= return . sudokuFromList . map guessUnkowns . zip (toList puzzleWithKnownsFilledIn)
                                             where guessUnkowns (k@(Known _),_) = k
                                                   guessUnkowns (_,r) = Unknown r

-- Give a list of input data, fill in the knowns 
fillInKnowns:: [(Int,Int,Int)] -> Matrix CellType
fillInKnowns startingData = foldl' (\curMatrix (val,x,y) -> setElem (Known $ val) (x,y) curMatrix ) startingZeroed startingData
                            where startingZeroed::Matrix CellType
                                  startingZeroed = fmap (\_ ->  None)$ zero sudokuSize sudokuSize

            
strToSudoku str =sudokuFromList $ map (\a -> if isDigit a then Known $ read [a] else None) str

