module Position
  ( Position(..)
  , validPosition
  , addPositions
  ) where

type Position = (Int, Int)

validPosition :: Position -> Bool
validPosition (x, y) = valid' x && valid' y
  where
    valid' value = value >= 0 && value <= 7

addPositions :: Position -> Position -> Position
addPositions (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
