module Chessboard
  ( Chessboard
  , initialBoard
  , changeColour
  , colour
  , pieceAtPosition
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
  show (Chessboard cells colour) = intercalate "\n" (map (\element -> intercalate " " (map showPiece element)) cells)

showPiece :: Maybe Piece -> String
showPiece piece =
  case piece of
    Just value -> show value
    Nothing    -> ""

changeColour :: Chessboard -> Chessboard
changeColour chessboard =
  Chessboard (pieces chessboard) $
  case (colour chessboard) of
    White -> Black
    Black -> White

possibleMoves :: Chessboard -> Piece -> [Move]
possibleMoves chessboard piece = []

{-
availableMoves :: Pieces -> Int -> Int -> [(Int, Int)]
availableMoves pieces x y =
  case ((cells pieces) !! y !! y of
    Pawn side -> [(0, 1)]


pieceMoves :: Piece ->-}
-- Change to x and y for available moves
--
