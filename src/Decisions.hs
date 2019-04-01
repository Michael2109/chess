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

validMoves :: Chessboard
validMoves chessboard = do
   let positions = getPiecePositions chessboard
   concat $ map (\position -> possibleMoves chessboard position) positions

alphaBetaNegamax :: Chessboard -> Int -> Int -> Int -> Int
alphaBetaNegamax chessboard depth alpha beta = do
    {-
https://github.com/Garee/jchess/blob/master/src/model/AI.java
      if ( board.isCheckmate() ) {
      return ( Integer.MIN_VALUE + 1 + this.depth - depth );
    } else if ( board.isStalemate() ) {
      return 0;
    } else if ( depth <= 0 ) {
      return ( evaluator.evaluate( board ) );
    }

    int score = Integer.MIN_VALUE + 1;
    for ( Move move : board.getValidMoves() ) {
      Board child = new Board( board );
      child.makeMove( move );
      score = -alphaBetaNegamax( child, depth - 1, -beta, -alpha );
      if ( score >= beta ) return ( score );
      if ( score > alpha ) alpha = score;
    }

    return ( alpha );
    -}


    let allValidMoves = validMoves chessboard

    let minimumValue = minBound :: Int

    let betaFound = find (\move -> do
         let newChessboard = makeMove chessboard move
         let score = -alphaBetaNegamax newChessboard (depth - 1) -beta -alpha
         if score >= beta then True
         else False) allValidMoves

    case betaFound of
      Just value -> value
      Nothing -> filter





