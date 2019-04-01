module Chessboard
  ( Chessboard
  , Piece(..)
  , PieceType(..)
  , initialBoard
  , changeColour
  , colour
  , pieceAtPosition
  , pieceColourAtPosition
  , getPiecePositions
  , getPiecePositionsWithColour
  , makeMove
  ) where

import Position
import Colour
import Move

import qualified Data.Char as C
import           Data.List

data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Eq)

instance Show PieceType where
  show Pawn   = "p"
  show Knight = "n"
  show Bishop = "b"
  show Rook   = "r"
  show Queen  = "q"
  show King   = "k"

data Piece =
  Piece Colour
        PieceType

instance Show Piece where
  show (Piece White t) = map C.toUpper $ show t
  show (Piece Black t) = show t

data Chessboard = Chessboard
  { pieces :: [[Maybe Piece]]
  , colour :: Colour
  }

pieceAtPosition :: Chessboard -> Position -> Maybe Piece
pieceAtPosition chessboard (x, y) = ((pieces chessboard) !! y) !! x

pieceColourAtPosition :: Chessboard -> Position -> Maybe Colour
pieceColourAtPosition chessboard (x, y) =
  case pieceAtPosition chessboard (x, y) of
    Just pieceOpt ->
      case pieceOpt of
        Piece colour _ -> Just colour
    Nothing -> Nothing

initialBoard :: Chessboard
initialBoard =
  Chessboard
    [ [ Just $ Piece Black Rook
      , Just $ Piece Black Knight
      , Just $ Piece Black Bishop
      , Just $ Piece Black Queen
      , Just $ Piece Black King
      , Just $ Piece Black Bishop
      , Just $ Piece Black Knight
      , Just $ Piece Black Rook
      ]
    , [ Just $ Piece Black Pawn
      , Just $ Piece Black Pawn
      , Just $ Piece Black Pawn
      , Just $ Piece Black Pawn
      , Just $ Piece Black Pawn
      , Just $ Piece Black Pawn
      , Just $ Piece Black Pawn
      , Just $ Piece Black Pawn
      ]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [ Just $ Piece White Pawn
      , Just $ Piece White Pawn
      , Just $ Piece White Pawn
      , Just $ Piece White Pawn
      , Just $ Piece White Pawn
      , Just $ Piece White Pawn
      , Just $ Piece White Pawn
      , Just $ Piece White Pawn
      ]
    , [ Just $ Piece White Rook
      , Just $ Piece White Knight
      , Just $ Piece White Bishop
      , Just $ Piece White Queen
      , Just $ Piece White King
      , Just $ Piece White Bishop
      , Just $ Piece White Knight
      , Just $ Piece White Rook
      ]
    ]
    White

instance Show Chessboard where
  show (Chessboard cells colour) = intercalate "\n" (map (intercalate " " . map showPiece) cells) ++ "\nMove: " ++ show colour

makeMove :: Chessboard -> Move -> Chessboard
makeMove chessboard move = do
  let originalPieces = pieces chessboard
  let originalColour = colour chessboard

  case move of
    Move (x1, y1) (x2, y2) -> do
      let pieceToMove = pieceAtPosition chessboard (x1, y1)
      let movedPiece = setPiece chessboard pieceToMove (x2, y2)
      let newChessboard = setPiece movedPiece Nothing (x1, y1)
      Chessboard (pieces newChessboard) (changeColour $ colour newChessboard)

setPiece :: Chessboard -> Maybe Piece -> Position -> Chessboard
setPiece chessboard piece (x, y) = do
    let originalPieces = pieces chessboard
    let newRow = replaceNth x piece (originalPieces !! y)
    Chessboard (replaceNth y newRow originalPieces) (colour chessboard)

replaceNth :: Int -> a -> [a] -> [a]
replaceNth _ _ [] = []
replaceNth n newVal (x:xs)
   | n == 0 = newVal:xs
   | otherwise = x:replaceNth (n-1) newVal xs

getPiecePositions :: Chessboard -> [Position]
getPiecePositions chessboard = concat $ map (\x -> map (\y -> (x, y)) [0..7]) [0..7]

getPiecePositionsWithColour :: Chessboard -> Colour -> [Position]
getPiecePositionsWithColour chessboard colour = do
  let positions = concat $ map (\x -> map (\y -> (x, y)) [0..7]) [0..7]
  filter (\position -> pieceColourAtPosition chessboard position == Just colour) positions

showPiece :: Maybe Piece -> String
showPiece piece =
  case piece of
    Just value -> show value
    Nothing    -> " "

changeColour :: Colour -> Colour
changeColour colour =
  case colour of
    White -> Black
    Black -> White
