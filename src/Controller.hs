module Controller where

import System.IO

import Chessboard
import Rules
import Position

startGame :: IO()
startGame = do
  putStrLn "Starting chess:"
  loop initialBoard
    -- Use recursion for loop
  where
    loop :: Chessboard -> IO ()
    loop currentChessboard = do
      input <- getLine
      if input /= "q"
        then do
          let newChessboard = changeColour currentChessboard
          print newChessboard
          putStrLn $ "Next move: " ++ show (colour newChessboard)

          print $ show (possibleMoves newChessboard (1, 0))
          loop newChessboard
        else return ()
