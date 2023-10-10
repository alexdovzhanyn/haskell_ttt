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
        move <- getPlayerMove gameState playerChar

        let nextPlayer = if playerChar == 'X' then 'O' else 'X'

        runGame (updateBoardWithMove gameState move playerChar) nextPlayer