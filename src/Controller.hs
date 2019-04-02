module Controller where

import System.IO
import Control.Monad

import Chessboard
import Rules
import Position
import Decisions
import Colour

startGame :: IO()
startGame = do
  putStrLn "Starting chess:"
  printChessboard initialBoard
  loop initialBoard
  where
    loop :: Chessboard -> IO ()
    loop chessboard = do
      input <- getLine
      Control.Monad.when (input /= "q") $ do
      
        putStrLn "Calculating best move"

        let newChessboard = applyBestMove chessboard
        printChessboard newChessboard
        loop newChessboard

printChessboard :: Chessboard -> IO ()
printChessboard chessboard = do
  putStrLn $ show chessboard
  putStrLn ""
