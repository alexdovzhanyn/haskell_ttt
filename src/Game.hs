module Game (
    GameState(..),
    BoardCoordinate(..),
    Player(..),
    Cell(..),
    initializeGameState,
    validateBoardCoordinate,
    updateBoardWithMove,
    checkGameFinished,
    getPlayerChar,
    getCellChar,
    opponentOf,
    endGame
) where

type GameState = [[Cell]]

data BoardCoordinate = BoardCoordinate (Int, Int) deriving (Show)

data Player = X | O deriving (Eq)

data Cell = Token Player | Empty deriving (Eq)

opponentOf :: Player -> Player
opponentOf X = O
opponentOf O = X

getPlayerChar :: Player -> Char
getPlayerChar X = 'X'
getPlayerChar O = 'O'

getCellChar :: Cell -> Char
getCellChar Empty = ' '
getCellChar (Token p) = getPlayerChar p

initializeGameState :: GameState
initializeGameState = replicate 3 . replicate 3 $ Empty

validateBoardCoordinate :: BoardCoordinate -> GameState -> Bool
validateBoardCoordinate (BoardCoordinate coordinate) gameState
    | (gameState !! fst coordinate) !! snd coordinate == Empty = True
    | otherwise = False

updateBoardWithMove :: GameState -> BoardCoordinate -> Player -> GameState
updateBoardWithMove gameState (BoardCoordinate (x, y)) player =
    updatedBoard
    where
        updatedBoard = map updateRow (zip [0..] gameState)

        updateRow (rowIndex, row)
            | rowIndex == x = zipWith updateCol [0..] row
            | otherwise = row

        updateCol colIndex col
            | colIndex == y = Token player
            | otherwise = col

checkGameFinished :: GameState -> Bool
checkGameFinished gameState
        | any (all (== Token X)) gameState = True
        | any (all (== Token O)) gameState = True
        | any (all (== Token X)) (transpose gameState) = True
        | any (all (== Token O)) (transpose gameState) = True
        | any (all (== Token X)) crosses = True
        | any (all (== Token O)) crosses = True
        | all (all (/= Empty)) gameState = True
        | otherwise = False
    where
        transpose ([]:_) = []
        transpose x = (map head x) : transpose (map tail x)
        crosses = [[ (gameState !! 0) !! 0, (gameState !! 1) !! 1, (gameState !! 2) !! 2 ], [ (gameState !! 0) !! 2, (gameState !! 1) !! 1, (gameState !! 2) !! 0]]

endGame :: IO ()
endGame = putStrLn "Game Over."