module Decisions where

import Data.List

import Chessboard
import Move
import Rules

bestMove :: Chessboard -> Move
bestMove chessboard = do
  let nextColour = (colour chessboard)
  let positions = getPiecePositionsWithColour chessboard nextColour
  let moves = concat $ map (\position -> possibleMoves chessboard position) positions
  let sortedMoves = map fst $ reverse $ sortOn snd $ map (\move -> (move, moveScore chessboard move)) moves

  sortedMoves !! 0

takenPoints :: Piece -> Int
takenPoints piece = case piece of
  Piece _ Rook -> 50
  Piece _ Knight -> 30
  Piece _ Bishop -> 30
  Piece _ Queen -> 90
  Piece _ King -> 900
  Piece _ Pawn -> 10

moveScore :: Chessboard -> Move -> Int
moveScore chessboard move = case move of
  Move startPos endPos -> do
    case pieceAtPosition chessboard endPos of
      Just takenPiece -> takenPoints takenPiece
      Nothing -> 0
