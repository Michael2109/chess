module Controller where

import System.IO

import Chessboard

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
          loop newChessboard
        else return ()
