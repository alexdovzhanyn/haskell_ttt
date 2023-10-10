module Main (main) where

import Interface
import Game

main :: IO ()
main = do
    playerChar <- greetPlayer
    runGame initializeGameState playerChar 

runGame :: GameState -> Char -> IO ()
runGame gameState playerChar
    | checkGameFinished gameState = do
        drawGameBoard gameState
        endGame
    | otherwise = do
        drawGameBoard gameState
        move <- getPlayerMove gameState

        runGame (updateBoardWithMove gameState move playerChar) playerChar