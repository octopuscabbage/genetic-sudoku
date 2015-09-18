module Main where
import Test.Hspec
import Test.QuickCheck
import Types
import Lib
import Data.Matrix
import AI.GeneticAlgorithm.Simple
import Control.Applicative

main :: IO ()
main = hspec $ do
  fitnessTests -- A nerds one true enemy


fitnessTests = parallel $ describe "Make sure fitness function works" $ do
  it "Make sure fitness function is 0 on correct sudoku" $ do
    fitness correctSudoku `shouldBe` 0
  it "Make sure row count works properly" $
    property $ \r t-> inRowNTimes t (unWrapCoord r) correctSudoku == 1
  it "Make sure col count works properly" $
    property $ \c t-> inColNTimes t (unWrapCoord c) correctSudoku == 1
  it "Make sure square count works properly" $
    property $ \x y t -> 1 == inSquareNTimes (unWrapCoord x, unWrapCoord y) t correctSudoku 
  it "incorrect should have -3 fitness" $ do
    fitness incorrectSudoku `shouldBe` -6
  it "incorret has wrong row" $ do
    inRowNTimes (Unknown 6) 1 incorrectSudoku == 2
  it "incorrect has wrong row" $ do
    inColNTimes (Unknown 6) 1 incorrectSudoku == 2
  it "things wrong with cell shold be 3" $ do
    thingsWrongWithCell incorrectSudoku (1,1) == 3


unWrapCoord (BoardCoord n) = n

data BoardCoord = BoardCoord Int deriving (Eq, Show, Read) --Used for constraining quickcheck
instance Arbitrary BoardCoord where
  arbitrary =  BoardCoord <$> (oneof $ [return 1, return 2, return 3, return 4, return 5, return 6, return 7, return 8, return 9])

instance Arbitrary CellType where
  arbitrary = do
    constructor <- oneof [return Known,return Unknown]
    value <- oneof [return 1, return 2, return 3, return 4, return 5, return 6, return 7, return 8, return 9]
    return $ (constructor value)

