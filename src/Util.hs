module Util where

import Data.Matrix
import AI.GeneticAlgorithm.Simple
import System.Random 
import Data.List (foldl')
import Types
import qualified Data.Vector as V
import qualified Control.Monad.Random as R
import Data.Char

--The size of rows and columns, as well as the max value for cell values
sudokuSize:: Int 
sudokuSize = 9


-- Given a flat list, turn it into a properly sized matrix
sudokuFromList:: [CellType] -> Matrix CellType
sudokuFromList = fromList sudokuSize sudokuSize

-- Is this matrix solved?
isSolved:: Matrix CellType -> Bool
isSolved m = all (\p -> 0 == thingsWrongWithCell m p) getAllCoordinates && (all (\x -> value x /= 0) $ toList m)

--Determing the number of things that are incorrect about this Matrix
thingsWrong:: Matrix CellType -> Int
thingsWrong m =  sum $ map (thingsWrongWithCell m) getAllCoordinates


--Starting at (0,0) to (9,9) produce a list of tuples of all the available coordinates
getAllCoordinates:: [(Int, Int)]
getAllCoordinates = [(x,y) | x <- [1..sudokuSize], y <- [1..sudokuSize]]

--Determine the number of things wrong with this cell
thingsWrongWithCell:: Matrix CellType -> (Int,Int) -> Int
thingsWrongWithCell m p@(x,y) =  length $filter (==False) $ [1 ==inRowNTimes cellAt y m, 1 == inColNTimes cellAt x m,1 == inSquareNTimes p cellAt m]
                          where cellAt = m ! p

-- | Return just the wrapped integer of this cell
value (Known a) = a
value (Unknown a) = a
value _ = 0


-- | How many times is the value of this cell in the list?
inListNTimes cell l =  (length $ filter (\a -> value a== value cell) l) 

-- | How many times is the value of cell in this row
inRowNTimes cell row m = inListNTimes cell $ V.toList $  getRow row m

-- | How many times is the value of cell in this column
inColNTimes cell col m = inListNTimes cell $ V.toList $ getCol col m

-- | How many times is the value of cell in this square
inSquareNTimes p@(x,y) cell m = inListNTimes cell $ toList $ getSubmatrix p m


-- | Helper function which produces the submatrix that the point is in
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



-- | Is the celltype a known 
isKnown:: CellType -> Bool
isKnown (Known _) = True
isKnown _ = False

-- | Print the matrix like an actual sudoku
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

-- | Given a string, produce a sudkou with the values as known values and . as None            
strToSudoku str =sudokuFromList $ map (\a -> if isDigit a then Known $ read [a] else None) str

