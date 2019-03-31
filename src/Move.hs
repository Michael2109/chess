module Move
  ( Move(..)
  ) where

import           Colour
import           Position

data Move
  = Move { startPosition :: Position
         , endPosition   :: Position }
  | KingSideCastling Colour
  | QueenSideCastling Colour
  deriving (Show)
