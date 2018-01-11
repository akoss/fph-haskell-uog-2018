{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (
    currentBoard
  , currentMap
  , printableMap
  , printableNextPlayer
  , move
  ) where

import Tafl.Core

currentBoard :: GameState -> IO Board
currentBoard gameState = board gameState

currentMap :: Board -> String
currentMap board = fields board

printableNextPlayer :: Char -> [Char]
printableNextPlayer a = (if a == 'O' then "Offense" else "Defense") ++ " is next"

printableLines :: String -> String

-- We're going to use recursion because that's just so much fancier than hardcoding 18 string positions
printableLines a = if length a > 9 then "\n|" ++ ((take 9 a) ++ "|" ++ printableLines (drop 9 a)) else ("\n|" ++ a)

printableMap  :: String -> String

printableMap a = "-----------" ++ printableLines a ++ "|\n-----------"

actuallyMove :: Board -> Int -> Int -> Board
actuallyMove board from to = board

canMove :: Board -> Int -> Int -> Bool
canMove board from to = True

move :: Board -> Int -> Int -> Maybe Board
move board from to = if canMove board from to then Just(actuallyMove board from to) else Nothing

