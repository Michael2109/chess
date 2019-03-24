module Board where

data PieceType = Pawn | Knight | Rook | Bishop | Queen | King deriving (Eq, Show)

data Side = White | Black deriving (Eq, Show)
