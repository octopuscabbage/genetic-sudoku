module GA.Evaluator where
import Data.Matrix
import AI.GeneticAlgorithm.Simple
import System.Random 
import Data.List (foldl')
import qualified Data.Vector as V
import Types
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad.Loops
import Control.DeepSeq
import System.Console.ANSI
import GAWrapper
import Control.Monad.Random
import Control.Monad.IO.Class

runWithoutLooking inputData = getStdGen >>= evalRandT $ runGAM herdSize mutationRate (generateStarting (fillInKnowns inputData)) (\m _ -> thingsWrong m == 0)

herdSize = 64
herds = 4           
minSize = 24
mutationRate = (0.1)

runWithHerds inputData = getStdGen >>= evalRandT $ do
  gen <- zeroGenerationM (generateStarting (fillInKnowns inputData)) herdSize
  winner <- newEmptyMVar
  mapM (\_ -> forkIO $ evalRandT $ do
            winner <-  runGAM herdSize mutationRate (generateStarting (fillInKnowns inputData)) (\a _ -> fitness a == 0)
            liftIO $ putMVar winner $!! the_winner
        ) [1..herds]


  resultant <- liftIO$ takeMVar winner
  liftIO $print $ "Solution: "
  print resultant



runAndPeak:: (String -> IO ()) -> [(Int,Int,Int)] -> IO ()
runAndPeak ioAction inputData = do
  g <- getStdGen
  let (zero,g') = zeroGeneration g (generateStarting (fillInKnowns inputData)) herdSize
  forkIO $ (runAndPeakWithSimilarMutation gen g >>= putMVar winner)
  whileM_ (isEmptyMvar winner) $ do
    current <- readMVar 
  resultant


ioCurrent:: (String -> IO ()) -> [Matrix CellType] -> IO ()
ioCurrent ioAction currentGeneration = do
  let best = head currentGeneration
  ioAction $ show $ fitness best
  ioAction $ prettyPrint best
  
  return ()

runAndPeak' curGen g = do
  current <- takeMVar curGen
  if (0 == (fitness $ head current)) then 
    return $ head current
  else  do
      let (nextGen,g') = nextGeneration g current herdSize mutationRate
      putMVar curGen nextGen
      runAndPeak' curGen g'



runAndPeakWithSimilarMutation:: RandomGen g => MVar [(Matrix CellType)] -> g -> IO (Matrix CellType)
runAndPeakWithSimilarMutation curGen g = do
  current <- takeMVar curGen
  if (0 == (fitness $ head current)) then do
    putMVar curGen current
    return $ head current
    else do
      let fitnesses = map fitness current
      let areAllEqual = (minimum fitnesses) == maximum fitnesses
      if areAllEqual then do
        let (mutated,g''') = (foldl' (\(cur, g') gene -> let (m, g'') = shuffle g' gene in (m:cur,g''))) ([],g) current
        let (nextGen,g') = nextGeneration g (current ++ mutated) herdSize mutationRate 
        putMVar curGen nextGen
        runAndPeakWithSimilarMutation curGen g'''
        else do
            let (nextGen,g') = nextGeneration g current herdSize mutationRate 
            putMVar curGen nextGen
            runAndPeakWithSimilarMutation curGen g'

shuffle g m1 = (sudokuFromList  $ map mutateGene $ zip3 (map (\p -> thingsWrongWithCell m1 p == 0) $ getAllCoordinates) infinite_list_of_one_to_length asList, snd $ next g)
                  where asList = toList m1
                        m_length = length asList
                        infinite_list_of_one_to_length = randomRs (1,9) g
                        mutateGene (False,r, gene@(Unknown a)) = Unknown (r) -- Only mutate cells which are not correct
                        mutateGene (_,_,cell) =  cell --Don't mutate known or not wrong cells cells



sudokuSize = 9

sudokuFromList = fromList 9 9 

instance Chromosome (Matrix CellType) where
  --Chose single point crossover, could look into other ones but i'm not a grad student so blah
  crossover g m1 m2 = randWith g $ do
    splitPoint <- getRandomR (0,lengthOfInput)
    return $ [sudokuFromList $ take splitPoint xs ++ drop splitPoint ys, sudokuFromList $ take splitPoint ys ++ drop splitPoint xs]
    where m1_list = toList m1 --for memoization purposes
          m2_list = toList m2
  --Bit string method, each element has a chance to go to a new random bit
  mutation g m1 = randWith g $ do
    elementToChange <- uniform $ getAllCoordinates
    newValue <- getRandomR (1,9)
    let elem = m ! elementToChange
    if not $ isKnown elem then
      return (setElem (Unkown newValue) elementToChange m1)
      else
      mutation g m1
  fitness m = fromIntegral $ negate $ thingsWrong m

