module Interface (
    greetPlayer,
    getPlayerMove,
    drawGameBoard
) where

import Data.Char (toLower, isDigit, isLetter)
import Data.List (intersperse, elemIndex)
import Data.Maybe (fromMaybe)
import Game (GameState(..), BoardCoordinate(..), validateBoardCoordinate)

greetPlayer :: IO Char
greetPlayer = do
    putStrLn "Welcome to Tic-Tac-Toe."
    
    character <- getPlayerDesiredCharacter

    putStrLn ("Okay, you'll be " ++ [character] ++ ". Player 2 will be " ++ if character == 'X' then ['O'] else ['X'])
    return character

getPlayerDesiredCharacter :: IO Char
getPlayerDesiredCharacter = do
    putStrLn "Player 1, do you want to be X or O?"
    userInput <- getLine

    -- Trim out whitespace, lowercase the input for easy matching later
    let trimmedInput = map toLower . unwords . words $ userInput
    
    case trimmedInput of
        "x" -> return 'X'
        "o" -> return 'O'
        _ -> do
            putStrLn ("\"" ++ userInput ++ "\" is not a valid input!")
            getPlayerDesiredCharacter

drawGameBoard :: GameState -> IO ()
drawGameBoard (GameState gameState) = do
    putStrLn "\n\n   A   B   C"
    let rows = intersperse "  ___________" $ (zipWith getBoardRow gameState [1..])
    mapM_ putStrLn rows
    putStrLn "\n\n"

getBoardRow :: [Char] -> Int -> String
getBoardRow [c1, c2, c3] idx = show idx ++ "  " ++ [c1] ++ " | " ++ [c2] ++ " | " ++ [c3]

getPlayerMove :: GameState -> Char -> IO BoardCoordinate
getPlayerMove gameState playerChar = do
    putStrLn ("Player " ++ [playerChar] ++ ", where would you like to place your token?")
    userInput <- getLine

    coordinate <- case convertStringToCoordinate (map toLower userInput) of
            Just tokenCoordinate -> return tokenCoordinate
            Nothing -> do
                putStrLn "\nThats not a valid token coordinate!"
                getPlayerMove gameState playerChar

    if validateBoardCoordinate coordinate gameState
    then return coordinate
    else do
        putStrLn "\nThere is already a token in that space!"
        getPlayerMove gameState playerChar

convertStringToCoordinate :: String -> Maybe BoardCoordinate
convertStringToCoordinate inputString
    | all (\x -> x `elem` rows ++ columns) inputString =
        case inputString of
            [x, y] | isDigit x && isDigit y -> Nothing
            [x, y] | isLetter x && isLetter y -> Nothing
            [x, y] | isDigit x -> Just (BoardCoordinate ((read [x]::Int) - 1, fromMaybe 0 (y `elemIndex` columns)))
            [x, y] | otherwise -> Just (BoardCoordinate ((read [y]::Int) - 1, fromMaybe 0 (x `elemIndex` columns)))
    | otherwise = Nothing
    where 
        rows = ['1', '2', '3']
        columns = ['a', 'b', 'c']
