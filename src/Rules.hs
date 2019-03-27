module Rules where

import Chessboard
import Move
import Position
import Colour

applyMove :: Chessboard -> Move -> Chessboard
applyMove chessboard move = chessboard

possibleMoves :: Chessboard -> Position -> [Move]
possibleMoves chessboard (x, y) = case (pieceAtPosition chessboard (x, y)) of
  Nothing -> []
  Just piece -> case piece of
    Piece colour Rook -> []
    Piece colour Knight -> do
      let newMoves = map (\offset -> Move (x, y) ((fst offset + x), snd offset + y)) [(-2, -1), (-2, 1), (2, -1), (2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2)]
      filter (\move -> validMove chessboard move) newMoves
    Piece colour Bishop -> []
    Piece colour Queen -> []
    Piece colour King -> []
    Piece colour Pawn -> []


validMove :: Chessboard -> Move -> Bool
validMove chessboard move = do
   case move of
     Move startPos endPos -> do
       if validPosition startPos && validPosition endPos
       then do
         let pieceAColour = pieceColourAtPosition chessboard startPos
         let pieceBColour = pieceColourAtPosition chessboard endPos

         case pieceAColour of
           Just aColour -> case pieceBColour of
             Just bColour -> aColour /= bColour
             Nothing -> True
       else
         False
     _ -> False
