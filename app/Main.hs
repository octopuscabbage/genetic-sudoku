module Main where

import Lib
import Control.Applicative

main :: IO ()
main =  runAndPeak testData2 >> return ()
