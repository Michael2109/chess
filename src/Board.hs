module Board where

data Side
  = White
  | Black
  deriving (Eq, Show)

data Piece
  = Pawn Side
  | Knight Side
  | Rook Side
  | Bishop Side
  | Queen Side
  | King Side
  | Empty
  deriving (Eq, Show)

initialBoard :: [[Piece]]
initialBoard =
  [ [Rook Black, Knight Black, Bishop Black, Queen Black, King Black, Bishop Black, Knight Black, Rook Black]
  , [Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
  , [Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White]
  , [Rook White, Knight White, Bishop White, Queen White, King White, Bishop White, Knight White, Rook White]
  ]
