module GAWrapper where
import AI.GeneticAlgorithm.Simple
import System.Random
import Control.Monad.Random
import Control.Monad.Random.Class
import Data.Functor.Identity

--This module just wraps some of the genetic algorithm functions in monadic random number generation

randWith::(RandomGen g, Monad m) => g -> RandT g m a -> m (a,g)
randWith = flip runRandT


--runGAM::(RandomGen g, Chromosome a)=> Int -> Double -> Rand g a -> (a -> Int -> Bool) -> Rand g a
runGAM pop mutation gen stop = liftRand $ \g -> runGA g pop mutation (starting gen) stop

starting::(RandomGen g, Chromosome a)=> RandT g Identity a -> g -> (a,g)
starting = runIdentity . runRand
                                              

zeroGenerationM::(RandomGen g,Chromosome a, Monad m) => RandT g m a -> Int -> RandT g m [a]
zeroGenerationM gen pop = liftRand $ \g -> zeroGeneration g (starting gen) pop 

nextGenerationM::(RandomGen g, Chromosome a, Monad m)=> [a] -> Int -> Double -> RandT g m [a]
nextGenerationM gen pop mutation = liftRand $ \g -> nextGeneration g gen pop mutation

