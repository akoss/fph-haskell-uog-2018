{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (
    printableMap
  , printableNextPlayer
  , readableMove
  ) where

import Tafl.Core
import Data.List
import Data.Maybe
import Data.Char

printableNextPlayer :: Char -> [Char]
printableNextPlayer a = (if a == 'O' then "Offense" else "Defense") ++ " is next"

-- We're going to use recursion because that's just so much fancier than hardcoding 18 string positions
printableLines :: Int -> String -> String
printableLines prefix a = if length a > 9 then "\n" ++ [chr prefix] ++ " |" ++ ((take 9 a) ++ "|" ++ (printableLines (prefix+1) (drop 9 a))) else ("\n" ++ [chr prefix] ++ " |" ++ a)

printableMap  :: String -> String
printableMap a = "\n   " ++ ['1'..'9'] ++ "\n  ___________" ++ printableLines (ord 'A') a ++ "|\n  ‾‾‾‾‾‾‾‾‾‾‾"

readableToInternalPosition :: String -> Int
readableToInternalPosition a = (ord (a !! 0) - (ord 'a')) * 9 + (read [a !! 1] :: Int) - 1

invertNextPlayer :: Char -> Char
invertNextPlayer player = if player == 'O' then 'D' else 'O'

swapAtIndex :: Int -> String -> Char -> String
swapAtIndex from old swapWith = (fst (splitAt from old)) ++ [swapWith] ++ tail(snd (splitAt from old))

actuallyMove :: Board -> Int -> Int -> Board
actuallyMove board from to = Board (swapAtIndex from (swapAtIndex to (fields board) (fields board !! from)) ' ') (invertNextPlayer $ nextPlayer board)

canMove :: Board -> Int -> Int -> Bool
canMove board from to = True

internalMove :: Board -> Int -> Int -> Maybe Board
internalMove board from to = if canMove board from to then Just(actuallyMove board from to) else Nothing

readableMove :: Board -> String -> String -> Maybe Board
readableMove board from to = internalMove board (readableToInternalPosition from) (readableToInternalPosition to)