module Controller where

import System.IO

import Board

startGame :: IO()
startGame = do
  putStrLn "Starting chess:"
  loop initialBoard

  where
    -- Use recursion for loop
    loop :: Chessboard -> IO ()
    loop currentChessboard = do
      input <- getLine
      if input /= "q"
        then do
          let newChessboard = changeColour currentChessboard
          print newChessboard
          putStrLn $ "Next move: " ++ (show $ (colour newChessboard))
          loop newChessboard
        else
          return ()
