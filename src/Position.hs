module Position (
  Position(..),
  validPosition)
  where

type Position = (Int, Int)

validPosition :: Position -> Bool
validPosition (x, y) = valid' x && valid' y
  where valid' value = value >= 0 && value <= 7


