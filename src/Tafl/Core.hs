{- |
This module defines several core data structures used by the game.
-}
module Tafl.Core
  ( GameState(..)
  , TaflError(..)
  , Command(..)
  , defaultBoardState
  , defaultGameState
  , initGameState
  , commandFromString
  , help_text
  , Board
  , GameState
  ) where

import System.Exit
import Data.List

-- | The core game state that captures the state of the board, and
-- whether we are playing a game or not.
--
-- You will need to extend this to present the board.

data Board = Board 
  { fields     :: [Char]
  , nextPlayer :: Char
  }

data GameState = GameState
  { inGame     :: Bool
  , inTestMode :: Bool
  , board      :: IO Board
  }

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

keepOnlyRelevantCharacters xs = [ x | x <- xs, (x `elem` " OGL") ]

boardStateFromIOString :: IO [String] -> IO Board
boardStateFromIOString a = do
  contents <- a
  let nextPlayer = (contents !! 0) !! 0
  putStrLn [nextPlayer]
  let fields = keepOnlyRelevantCharacters (contents !! 1 ++ contents !! 2 ++ contents !! 3)
  putStrLn fields
  return (Board fields nextPlayer)

dummyIOBoard :: Board -> IO Board
dummyIOBoard a = return a

boardStateFromFile :: FilePath -> IO Board 
boardStateFromFile a = boardStateFromIOString $ readLines a

-- To maintain easy compatibility with file-based game states, we pretend that the defaultBoard is from a file: 
defaultBoardState :: IO Board
-- As on Figure 4 of FP(H) Practical 2 requirements:
defaultBoardState = dummyIOBoard $ Board "   OOO       O        G    O   G   OOOGGLGGOOO   G   O    G        O       OOO   " 'O'

defaultGameState :: GameState
defaultGameState = GameState False False defaultBoardState

-- Finish initGameState to read a board state from file.
initGameState :: Maybe FilePath
              -> Bool
              -> IO (Either TaflError GameState)
initGameState Nothing  b = pure $ Right $ GameState False b defaultBoardState
initGameState (Just f) b = pure $ Right $ GameState False b $ boardStateFromFile f

-- | Errors encountered by the game, you will need to extend this to capture *ALL* possible errors.
data TaflError = InvalidCommand String
               | UnknownCommand
               | NotYetImplemented

-- | REPL commands, you will need to extend this to capture all permissible REPL commands.
data Command = Help
             | Exit
             | Start
             | Stop

-- | Try to construct a command from the given string.
commandFromString :: String -> Either TaflError Command
commandFromString (':':rest) =
  case words rest of
    ["help"] -> Right Help
    ["exit"] -> Right Exit
    ["start"] -> Right Start
    ["stop"]  -> Right Stop

    -- You need to specify how to recognise the remaining commands and their arguments here.

    _         -> Left UnknownCommand

commandFromString _  = Left UnknownCommand


help_text :: String
help_text = unlines $
     [ "Tafl Help text:", ""]
  ++ map prettyCmdHelp
       [ ("help",  "Displays this help text." )
       , ("exit",  "Exits the Command Prompt.")
       , ("start", "Initiates a game."        )
       , ("stop",  "Stops a game."            )
       ]
  where
    prettyCmdHelp :: (String, String) -> String
    prettyCmdHelp (cmd, help) = concat ["\t:", cmd, "\t", " "] ++ help
