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

data Pieces = Pieces
  { cells :: [[Piece]]
  } deriving (Show)

initialBoard :: Pieces
initialBoard =
  Pieces
    [ [Rook Black, Knight Black, Bishop Black, Queen Black, King Black, Bishop Black, Knight Black, Rook Black]
    , [Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black, Pawn Black]
    , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    , [Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty]
    , [Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White, Pawn White]
    , [Rook White, Knight White, Bishop White, Queen White, King White, Bishop White, Knight White, Rook White]
    ]

availableMoves :: Pieces -> (Int, Int) -> [(Int, Int)]
availableMoves pieces position =
  case ((cells pieces) !! (snd position) !! (fst position)) of
    Pawn side -> [(0, 1)]
