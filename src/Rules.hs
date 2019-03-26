module Rules where

import Chessboard
import Move
import Position

applyMove :: Chessboard -> Move -> Chessboard
applyMove chessboard move = chessboard

possibleMoves :: Chessboard -> Position -> [Move]
possibleMoves chessboard (x, y) = [] --(pieceAtPosition chessboard (x, y))
