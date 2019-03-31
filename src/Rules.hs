module Rules where

import           Chessboard
import           Colour
import           Move
import           Position

applyMove :: Chessboard -> Move -> Chessboard
applyMove chessboard move = chessboard

possibleMoves :: Chessboard -> Position -> [Move]
possibleMoves chessboard position =
  case pieceAtPosition chessboard position of
    Nothing -> []
    Just piece ->
      case piece of
        Piece colour Rook   -> rookMoves chessboard position colour
        Piece colour Knight -> knightMoves chessboard position colour
        Piece colour Bishop -> bishopMoves chessboard position colour
        Piece colour Queen  -> queenMoves chessboard position colour
        Piece colour King   -> kingMoves chessboard position colour
        Piece colour Pawn   -> pawnMoves chessboard position colour

movesInDirection :: Chessboard -> Position -> Position -> Position -> Colour -> Int -> [Move]
movesInDirection chessboard startPosition currentPosition direction colour movesLeft = do
  let nextPosition = addPositions currentPosition direction
  let nextMove = Move startPosition nextPosition
  if movesLeft /= 0 && validPosition nextPosition
    then if takingAPiece chessboard nextMove
           then [nextMove]
           else if movingToEmptySpace chessboard nextMove
                  then nextMove :
                       movesInDirection chessboard startPosition nextPosition direction colour (movesLeft - 1)
                  else []
    else []

rookMoves :: Chessboard -> Position -> Colour -> [Move]
rookMoves chessboard position colour = do
  let directions = [(0, 1), (1, 0), (0, -1), (-1, 0)]
  concat $ map (\direction -> movesInDirection chessboard position position direction colour 8) directions

knightMoves :: Chessboard -> Position -> Colour -> [Move]
knightMoves chessboard (x, y) colour = do
  let newMoves =
        map
          (\offset -> Move (x, y) ((fst offset + x), snd offset + y))
          [(-2, -1), (-2, 1), (2, -1), (2, 1), (-1, -2), (-1, 2), (1, -2), (1, 2)]
  filter
    (\move -> (validPosition $ endPosition move) && (takingAPiece chessboard move || movingToEmptySpace chessboard move))
    newMoves

bishopMoves :: Chessboard -> Position -> Colour -> [Move]
bishopMoves chessboard position colour = do
  let directions = [(1, 1), (1, -1), (-1, -1), (-1, 1)]
  concat $ map (\direction -> movesInDirection chessboard position position direction colour 8) directions

queenMoves :: Chessboard -> Position -> Colour -> [Move]
queenMoves chessboard position colour = do
  let directions = [(0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, -1), (-1, 1)]
  concat $ map (\direction -> movesInDirection chessboard position position direction colour 8) directions

kingMoves :: Chessboard -> Position -> Colour -> [Move]
kingMoves chessboard position colour = do
  let directions = [(0, 1), (1, 0), (0, -1), (-1, 0), (1, 1), (1, -1), (-1, -1), (-1, 1)]
  concat $ map (\direction -> movesInDirection chessboard position position direction colour 1) directions

pawnMoves :: Chessboard -> Position -> Colour -> [Move]
pawnMoves chessboard position colour = do
  let direction =
        if colour == White
          then (0, -1)
          else (0, 1)
  let forwardMoves = movesInDirection chessboard position position direction colour 1

  let attackDirections =
        if colour == White
          then [(-1, -1), (1, -1)]
          else [(-1, 1), (1, 1)]

  let attackMoves =
        filter (takingAPiece chessboard) $ concat $ map (\attackDirection -> movesInDirection chessboard position position attackDirection colour 1) attackDirections

  filter
    (\move ->
       case move of
         Move startPos endPos -> pieceColourAtPosition chessboard endPos == Nothing)
    forwardMoves ++ attackMoves

takingAPiece :: Chessboard -> Move -> Bool
takingAPiece chessboard move =
  case move of
    Move startPos endPos ->
      (validPosition startPos && validPosition endPos) &&
      (do let pieceAColour = pieceColourAtPosition chessboard startPos
          let pieceBColour = pieceColourAtPosition chessboard endPos
          case pieceAColour of
            Just aColour ->
              case pieceBColour of
                Just bColour -> aColour /= bColour
                Nothing      -> False)
    _ -> False

movingToEmptySpace :: Chessboard -> Move -> Bool
movingToEmptySpace chessboard move =
  case move of
    Move startPos endPos ->
      case pieceAtPosition chessboard endPos of
        Nothing -> True
        Just _  -> False
