module Game (
    GameState(..),
    BoardCoordinate(..),
    initializeGameState,
    validateBoardCoordinate,
    updateBoardWithMove,
    checkGameFinished,
    endGame
) where

data GameState = GameState [[Char]]

data BoardCoordinate = BoardCoordinate (Int, Int) deriving (Show)

initializeGameState :: GameState
initializeGameState = GameState $ replicate 3 . replicate 3 $ ' '

validateBoardCoordinate :: BoardCoordinate -> GameState -> Bool
validateBoardCoordinate (BoardCoordinate coordinate) (GameState gameState)
    | (gameState !! fst coordinate) !! snd coordinate == ' ' = True
    | otherwise = False

updateBoardWithMove :: GameState -> BoardCoordinate -> Char -> GameState
updateBoardWithMove (GameState gameState) (BoardCoordinate (x, y)) playerChar  =
    GameState updatedBoard
    where
        updatedBoard = map updateRow (zip [0..] gameState)

        updateRow (rowIndex, row)
            | rowIndex == x = zipWith updateCol [0..] row
            | otherwise = row

        updateCol colIndex col
            | colIndex == y = playerChar
            | otherwise = col

checkGameFinished :: GameState -> Bool
checkGameFinished (GameState gameState)
        | any (all (== 'X')) gameState = True
        | any (all (== 'O')) gameState = True
        | any (all (== 'X')) (transpose gameState) = True
        | any (all (== 'O')) (transpose gameState) = True
        | otherwise = False
    where
        transpose ([]:_) = []
        transpose x = (map head x) : transpose (map tail x)

endGame :: IO ()
endGame = putStrLn "Game Over."