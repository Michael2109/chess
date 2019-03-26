module Position (
  Position,
  valid)
  where

type Position = (Int, Int)

valid :: Position -> Bool
valid (x, y) = valid' x && valid' y
  where valid' value = value >= 0 && value <= 7
