module Main where

import Lib
import Control.Applicative

main :: IO ()
main =  runAndPeak testData >> return ()
