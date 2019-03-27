module Move
  (Move(..))
  where

import Position
import Colour

data Move = Move Position Position
  | KingSideCastling Colour
  | QueenSideCastling Colour
  deriving (Show)
