{- |

The `Process` module implements the game commands.

-}
module Tafl.Process
  ( processCommand
  , processCommandStr
  , printError
  ) where

import System.Exit

import Tafl.Core
import Tafl.Logic
import Data.Maybe

-- | Process user commands and updates the GameState.
-- Returns a `TaflError`
processCommand :: GameState
               -> Command
               -> String
               -> IO (Either TaflError GameState)
processCommand st Help str = do
  putStrLn help_text
  pure $ Right st
processCommand st Exit str = do
  putStrLn "Good Bye!"
  exitWith ExitSuccess

processCommand st Start str = do
   if inGame st
    then do 
       pure $ Left (CommandCannotBeUsed)
    else do
       let newSt = st {inGame=True}
       putStrLn "Starting Game."
       b <- board st
       putStrLn $ printableMap $ fields b
       putStrLn $ (printablePlayer $ nextPlayer b)  ++ " is next"

       pure $ Right newSt

processCommand st Stop str = do
   if inGame st
    then do
     let newSt = st {inGame=False}
     putStrLn "Stopping Game."
     pure $ Right newSt
    else do
      pure $ Left (CommandCannotBeUsed)

processCommand st Move str = do
  b <- board st
  if inGame st
    then do    
      let newBoard = readableMove b (words str !! 1) (words str !! 2)
      if isNothing newBoard
        then do 
          pure $ Left (InvalidMove)
        else do
          let finalBoard = (boardToSave b newBoard)
          let newSt = st {board = pure $ finalBoard}    
          putStrLn $ printableMap $ fields finalBoard
          putStrLn $ (printablePlayer $ nextPlayer finalBoard)  ++ " is next"
          pure $ Right newSt
    else do
      pure $ Left (CommandCannotBeUsed)
-- The remaining commands are to be added here.

processCommand st _ str = pure $ Left (UnknownCommand)

boardToSave :: Board -> Maybe Board -> Board
boardToSave oldBoard newBoard = case newBoard of
    Nothing -> oldBoard
    Just newBoard -> newBoard

-- | Process a user given command presented as a String, and update
-- the GameState.
processCommandStr :: GameState
                  -> String
                  -> IO (Either TaflError GameState)
processCommandStr st str =
  case commandFromString str of
    Left err   -> pure (Left err)
    Right cmd' -> processCommand st (fst cmd') str


-- | Print an Error to STDOUT.
printError :: TaflError -> IO ()
printError (UnknownCommand) = do
  putStrLn "The entered command was not recognised."
printError (InvalidMove) = do
  putStrLn "Invalid Move!"
printError (CommandCannotBeUsed) = do
  putStrLn "The command cannot be used."
printError (InvalidCommand msg) = do
  putStrLn "You entered an invalid command:"
  putStr "\t"
  putStrLn msg
