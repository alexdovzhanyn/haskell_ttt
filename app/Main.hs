module Main (main) where

import Interface
import Game

main :: IO ()
main = do
    playerChar <- greetPlayer
    runGame initializeGameState playerChar 

runGame :: GameState -> Player -> IO ()
runGame gameState player
    | checkGameFinished gameState = do
        drawGameBoard gameState
        endGame
    | otherwise = do
        drawGameBoard gameState
        move <- getPlayerMove gameState player

        runGame (updateBoardWithMove gameState move player) (opponentOf player)