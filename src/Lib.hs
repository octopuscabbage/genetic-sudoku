module Lib (
  module Lib,
  module BackTracker.BackTracker,
  module Util
           )where
import BackTracker.BackTracker
import Util
import Types

-- | This module just exports other modules and some testing data

testData:: [(Int, Int, Int)]
testData = [(9,2,1), (3,5,1), (1,8,1), (7,2,2), (2,6,2), (4,9,2),(6,5,3),(8,6,3),(3,8,3), (7,9,3), (5,1,4), (3,2,4), (6,3,4), (1,6,4), (9,7,4), (8,8,4), (1,1,5), (7,3,5), (3,7,5), (4,8,5), (6,9,5), (2,2,6), (8,5,6), (7,8,6), (7,1,7), (5,3,7), (1,4,7), (9,5,7), (4,6,7), (2,1,8), (1,2,8), (3,3,8), (8,4,8), (5,6,8),(6,8,8),(4,2,9),(3,6,9), (1,9,9)]

testData2::[(Int,Int,Int)]
testData2 = [(5,2,1),(4,4,1),(8,5,1),(2,7,1),(9,8,1),(1,1,2),(7,2,4),(4,3,3),(3,3,7),(6,4,6),(1,7,4),(8,1,5),(3,2,5),(4,8,5),(6,9,5),(9,3,6),(1,4,6),(2,3,7),(6,7,7),(8,6,8),(2,9,8),(4,2,9),(5,3,9),(7,5,9),(9,6,9),(1,8,9)]

testData3::[(Int,Int,Int)]
testData3 = [(1,1,3),(4,1,6),(7,1,7),(3,1,8),(4,2,2),(7,2,3),(8,2,9),(1,3,4),(8,3,6),(5,3,7),(6,4,2),(5,4,3),(7,4,6),(9,4,8),(6,5,5),(2,2,6),(3,6,4),(1,6,7),(6,6,8),(7,7,3),(6,7,6),(2,6,2),(2,7,4),(1,8,1),(5,8,8),(7,8,9),(4,9,2),(9,9,3),(7,9,4),(3,9,7)]
correctSudoku = sudokuFromList [u 5, u 3, u 4, u 6, u 7, u 8, u 9, u 1, u 2, u 6, u 7, u 2, u 1, u 9, u 5, u 3, u 4, u 8, u 1, u 9,u 8,u 3, u 4, u 2, u 5, u 6, u 7,u 8,u 5, u 9,u 7, u 6, u 1, u 4, u 2, u 3, u 4, u 2, u 6, u 8, u 5, u 3, u 7, u 9, u 1, u 7,u 1, u  3,u 9, u 2,u 4, u 8,u 5,u 6,u 9,u 6, u 1, u 5, u 3, u 7, u 2 ,u 8, u 4, u 2, u 8, u 7, u 4, u 1, u 9,u 6, u 3, u 5,u 3, u 4, u 5, u 2, u 8, u 6, u 1, u 7, u 9]
 where u = Unknown

--First cell is wrong, should be 5
incorrectSudoku = sudokuFromList [u 6, u 3, u 4, u 6, u 7, u 8, u 9, u 1, u 2, u 6, u 7, u 2, u 1, u 9, u 5, u 3, u 4, u 8, u 1, u 9,u 8,u 3, u 4, u 2, u 5, u 6, u 7,u 8,u 5, u 9,u 7, u 6, u 1, u 4, u 2, u 3, u 4, u 2, u 6, u 8, u 5, u 3, u 7, u 9, u 1, u 7,u 1, u  3,u 9, u 2,u 4, u 8,u 5,u 6,u 9,u 6, u 1, u 5, u 3, u 7, u 2 ,u 8, u 4, u 2, u 8, u 7, u 4, u 1, u 9,u 6, u 3, u 5,u 3, u 4, u 5, u 2, u 8, u 6, u 1, u 7, u 9]
                  where u = Unknown
