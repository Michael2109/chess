module Decisions(applyBestMove) where

import Data.List
import Data.List (sortBy)
import Data.Function (on)

import Chessboard
import Move
import Rules

applyBestMove :: Chessboard -> Chessboard
applyBestMove chessboard = do
    let chessboards = possibleBoards chessboard
    (map snd $ reverse $ sortBy (flip compare `on` fst)  (map (\chessboard -> (negamax chessboard 5, chessboard)) chessboards)) !! 0


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

possibleBoards :: Chessboard -> [Chessboard]
possibleBoards chessboard = do
   let positions = getPiecePositions chessboard
   let moves = concat $ map (\position -> possibleMoves chessboard position) positions
   map (makeMove chessboard) moves

-- Returned colour is the winner
checkmate :: Chessboard -> Maybe Colour
checkmate chessboard = do

   -- If win then return winning colour
   let whitePieces = map (pieceAtPosition chessboard) $ getPiecePositionsWithColour chessboard White
   let blackPieces = map (pieceAtPosition chessboard) $ getPiecePositionsWithColour chessboard Black

   if not $ kingAlive whitePieces
     then Just Black
     else if not $ kingAlive blackPieces
       then Just White
       else Nothing

   where
      kingAlive pieces = do
        let king = find (\foundPiece -> do
             case foundPiece of
               Just piece -> case piece of
                 Piece _ King -> True
                 _ -> False
             ) pieces
        case king of
          Just _ -> True
          Nothing -> False

staleMate :: Chessboard -> Bool
staleMate chessboard = do
  -- When same move occurs three times

  -- When the next player has no available moves

  -- 50 successive moves maqde by both players containing no capture or pawn moves
  False

negamax :: Chessboard -> Int -> Int
negamax chessboard depth = do

    if depth == 0 then 0
    else

      case checkmate chessboard of
        Just colour -> case colour of
          White -> 10
          Black -> -10
        Nothing -> do

            if staleMate chessboard
            then 0
            else do

              let scores = map (\possibleBoard -> -(negamax possibleBoard $ depth - 1)) $ possibleBoards chessboard
              maximum scores
