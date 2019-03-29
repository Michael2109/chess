module Decisions where

import Chessboard
import Move
import Rules

bestMove :: Chessboard -> Move
bestMove chessboard = do
  let nextColour = (colour chessboard)
  let positions = getPiecePositionsWithColour chessboard nextColour
  let moves = concat $ map (\position -> possibleMoves chessboard position) positions
  moves !! 0
