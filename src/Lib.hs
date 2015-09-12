{-# LANGUAGE FlexibleInstances #-}
module Lib where
import Data.Matrix
import AI.GeneticAlgorithm.Simple
import System.Random 
import Data.List (foldl')
import qualified Data.Vector as V
import Control.DeepSeq


sudokuSize = 9

data CellType = Known Int
                | Unkown Int deriving (Eq, Read)
instance Show CellType where
  show (Known a) = "K " ++ show a  ++ " "
  show (Unkown a) = "U " ++ show a ++ " "

instance NFData CellType where
  rnf (Known a) =  (Known $ a `seq` a) `seq` () -- I think I did this wrong
  rnf (Unkown a) = (Unkown $ a `seq` a) `seq` ()

sudokuFromList = fromList 9 9 

instance Chromosome (Matrix CellType) where
  --Chose single point crossover, could look into other ones but i'm not a grad student so blah
  crossover g m1 m2 = listCrossover (m1_list) (m2_list) --make sure crossover produces a matrix of equal size
                      where listCrossover xs ys = ([sudokuFromList $ take splitPoint xs ++ drop splitPoint ys, sudokuFromList $ take splitPoint ys ++ drop splitPoint xs],nextGen)
                            m1_list = toList m1 --for memoization purposes
                            m2_list = toList m2
                            randomValAndGen = randomR (0,lengthOfInput) g
                            splitPoint = fst $ randomValAndGen
                            nextGen = snd $ randomValAndGen
                            lengthOfInput = length m1_list
  --bit string method, each element has a chance to go to a new random bit
  mutation g m1 = (sudokuFromList  $ map mutateGene $ zip3 (map (\p -> thingsWrongWithCell m1 p == 0) $ getAllCoordinates) infinite_list_of_one_to_length asList, snd $ next g)
                  where asList = toList m1
                        m_length = length asList
                        infinite_list_of_one_to_length = randomRs (1,9) g
                        mutateGene (False,r, gene@(Unkown a)) = Unkown (r) -- Only mutate cells which are not correct
                        mutateGene (_,_,cell) =  cell --Don't mutate known or not wrong cells cells
  fitness m = fromIntegral $ negate $ thingsWrong m

                            --
thingsWrong:: Matrix CellType -> Int
thingsWrong m =  sum $ map (thingsWrongWithCell m) getAllCoordinates

getAllCoordinates:: [(Int, Int)]
getAllCoordinates = [(x,y) | x <- [1..sudokuSize], y <- [1..sudokuSize]]

thingsWrongWithCell:: Matrix CellType -> (Int,Int) -> Int
thingsWrongWithCell m p@(x,y) 
  | (isKnown cellAt) = 0
  | otherwise = length $filter (==True) $ [rowSumIsNot45 || inRowTwoOrMoreTimes, colSumIsNot45 || inColTwoOrMoreTimes,inSquareTwoOrMoreTimes]
                          where cellAt = m ! p
                                inVecTwoOrMoreTimes vec =  (V.length $ V.filter (==cellAt) vec) > 1
                                rowSumIsNot45 = V.sum $ getRow x m
                                colSumIsNot45 = V.sum $ getRow y m
                                inRowTwoOrMoreTimes = inVecTwoOrMoreTimes $ getRow x m
                                inColTwoOrMoreTimes = inVecTwoOrMoreTimes $ getCol y m
                                inSpecificSquareTwoOrMoreTimes startRow endRow startCol endCol =  (length $ filter(==cellAt) $ toList $ submatrix startRow endRow startCol endCol m) > 1
                                inSquareTwoOrMoreTimes
                                  | (x <= 3 && y <= 3 ) = inSpecificSquareTwoOrMoreTimes 1 3 1 3 
                                  | (x > 3 && x <= 6 && y <= 3) = inSpecificSquareTwoOrMoreTimes 3 6 1 3 
                                  | (x > 6 && y <= 3) = inSpecificSquareTwoOrMoreTimes 6 9 1 3 
                                  | (x <= 3 && y > 3 && y <= 6) =inSpecificSquareTwoOrMoreTimes 1 3 3 6 
                                  | (x >3 && x<= 6 && y >3 && y <= 6) =inSpecificSquareTwoOrMoreTimes 3 6 3 6 
                                  | (x >6 && y > 3 && y <= 6) = inSpecificSquareTwoOrMoreTimes 6 9 3 6 
                                  | (x <= 3 && y >6) = inSpecificSquareTwoOrMoreTimes 1 3 6 9
                                  | (x >3 && x <= 6 && y > 6) = inSpecificSquareTwoOrMoreTimes 3 6 6 9
                                  | (x > 6 && y > 6) = inSpecificSquareTwoOrMoreTimes 6 9 6 9
                                  | otherwise = error $ "Not found in Matrix " ++ show x ++ " "++ show y
                                                          

isKnown:: CellType -> Bool
isKnown (Known _) = True
isKnown _ = False

--Given a puzzle with the knowns filled in, use the random iterator g and generate a bunch of randomly filled startind ata
--also advance the rng because what is state monad
generateStarting:: RandomGen g => Matrix CellType -> g -> (Matrix CellType, g)
generateStarting puzzleWithKnownsFilledIn g= (sudokuFromList $ map guessUnkowns $ zip (randomRs (1,9) g) (toList puzzleWithKnownsFilledIn), snd $ next g)
                                             where guessUnkowns (_,k@(Known _)) = k
                                                   guessUnkowns (r,_) = Unkown r

-- Give a list of input data, fill in the knowns 
-- The tuple is val, x, y. It's fliped in the lambda expression to make 0,0 the top left corner
fillInKnowns:: [(Int,Int,Int)] -> Matrix CellType
fillInKnowns startingData = foldl' (\curMatrix (val,y,x) -> setElem (Known $ val) (x,y) curMatrix ) startingZeroed startingData
                            where startingZeroed::Matrix CellType
                                  startingZeroed = fmap (\_ ->  Unkown 0)$ zero sudokuSize sudokuSize

runWithoutLooking inputData = do
  g <- getStdGen
  return $ runGA (g) 8 0.1 (generateStarting (fillInKnowns inputData)) (\m _ -> thingsWrong m == 0)

runAndPeak inputData = do
  g <- getStdGen
  let (zero,g') = zeroGeneration g (generateStarting (fillInKnowns inputData)) 64
  runAndPeak' zero g'

runAndPeak' curGen g = do
  print "--- NEW ITERATION --"
  mapM_ (\m -> (print $ "Distance from Solution: " ++ (show $ fitness m)) >> (print m)  ) $ reverse curGen
  if (0 == (thingsWrong $ head curGen)) then do
    print "--- FOUND SOLUTION ---"
    print $ head curGen
    return $ head curGen
    else
      let (nextGen,g') = nextGeneration g curGen 64 (0.3)  in
      runAndPeak' nextGen g'

testData:: [(Int, Int, Int)]
testData = [(9,2,1), (3,5,1), (1,8,1), (7,2,2), (2,6,2), (4,9,2),(6,5,3),(8,6,3),(3,8,3), (7,9,3), (5,1,4), (3,2,4), (6,3,4), (1,6,4), (9,7,4), (8,8,4), (1,1,5), (7,3,5), (3,7,5), (4,8,5), (6,9,5), (2,2,6), (8,5,6), (7,8,6), (7,1,7), (5,3,7), (1,4,7), (9,5,7), (4,6,7), (2,1,8), (1,2,8), (3,3,8), (8,4,8), (5,6,8),(6,8,8),(4,2,9),(3,6,9), (1,9,9)]
