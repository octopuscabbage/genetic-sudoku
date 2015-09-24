module Types where

import Control.DeepSeq

-- | Represents the three states a cell can be
data CellType = Known Int -- | It's been known since the begining
                | Unknown Int -- | It's been guessed
                | None -- | Hasn't been guessed yet
                deriving (Eq, Read, Ord) 
                

instance Show CellType where -- | Makes them all print at a fixed width
  show (Known a) = "K " ++ show a
  show (Unknown a) = "U " ++ show a
  show (None) = "N ."

instance NFData CellType where -- | Allows matricies to be deepseqed
  rnf (Known a) =  (Known $ a `seq` a) `seq` () -- I think I did this wrong
  rnf (Unknown a) = (Unknown $ a `seq` a) `seq` ()
  rnf (None) = None `seq` ()

