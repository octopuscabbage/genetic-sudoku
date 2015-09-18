module Types where

import Control.DeepSeq

data CellType = Known Int
                | Unknown Int
                | None deriving (Eq, Read, Ord)
                

instance Show CellType where
  show (Known a) = "K " ++ show a
  show (Unknown a) = "U " ++ show a
  show (None) = "N ."

instance NFData CellType where
  rnf (Known a) =  (Known $ a `seq` a) `seq` () -- I think I did this wrong
  rnf (Unknown a) = (Unknown $ a `seq` a) `seq` ()
  rnf (None) = None `seq` ()

