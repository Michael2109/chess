module Board where

import qualified Data.Char as C
import Data.List

data Colour
  = White
  | Black
  deriving (Eq, Show)

data PieceType
  = Pawn
  | Knight
  | Bishop
  | Rook
  | Queen
  | King
  deriving (Eq)

instance Show PieceType where
  show Pawn = "p"
  show Knight = "n"
  show Bishop = "b"
  show Rook = "r"
  show Queen = "q"
  show King = "k"

data Piece = Piece Colour PieceType

instance Show Piece where
  show (Piece White t) = map C.toUpper $ show t
  show (Piece Black t) = show t

data Board = Board [[Maybe Piece]] Colour

initialBoard :: Board
initialBoard =
  Board
    [ [Just $ Piece Black Rook, Just $ Piece Black Knight, Just $ Piece Black Bishop, Just $ Piece Black Queen, Just $ Piece Black King, Just $ Piece Black Bishop, Just $ Piece Black Knight, Just $ Piece Black Rook]
    , [Just $ Piece Black Pawn, Just $ Piece Black Pawn, Just $ Piece Black Pawn, Just $ Piece Black Pawn, Just $ Piece Black Pawn, Just $ Piece Black Pawn, Just $ Piece Black Pawn, Just $ Piece Black Pawn]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing]
    , [Just $ Piece White Pawn, Just $ Piece White Pawn, Just $ Piece White Pawn, Just $ Piece White Pawn, Just $ Piece White Pawn, Just $ Piece White Pawn, Just $ Piece White Pawn, Just $ Piece White Pawn]
    , [Just $ Piece White Rook, Just $ Piece White Knight, Just $ Piece White Bishop, Just $ Piece White Queen, Just $ Piece White King, Just $ Piece White Bishop, Just $ Piece White Knight, Just $ Piece White Rook]
    ]
    White

instance Show Board where
  show (Board cells colour) = intercalate "\n"  (map (\element -> intercalate " " (map showPiece element)) cells)

showPiece :: Maybe Piece -> String
showPiece piece = case piece of
    Just value -> show value
    Nothing -> ""

{-
availableMoves :: Pieces -> Int -> Int -> [(Int, Int)]
availableMoves pieces x y =
  case ((cells pieces) !! y !! y of
    Pawn side -> [(0, 1)]


pieceMoves :: Piece ->-}
-- Change to x and y for available moves

--
