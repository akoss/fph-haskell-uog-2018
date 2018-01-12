{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (
    printableMap
  , printablePlayer
  , readableMove
  ) where

import Tafl.Core
import Data.List
import Data.Maybe
import Data.Char

boardSize = 9
throne = 40

isOwnerOfPiece :: Char -> Char -> Bool
isOwnerOfPiece player piece = if player == 'D' then piece `elem` ['L','G'] else piece == 'O'

isEmptyField :: Char -> Bool
isEmptyField field = field == ' '

isVertical :: Int -> Int -> Bool
isVertical from to = (from `mod` boardSize) == (to `mod` boardSize)

isHorizontal :: Int -> Int -> Bool
isHorizontal from to = to >= (from - (from `mod` boardSize)) && to < (from - (from `mod` boardSize) + boardSize)

isPathClear :: Board -> Int -> Int -> Bool
isPathClear board from to = if isVertical from to then isVerticalPathClear board (increaseFromInVerticalPathClear from to) to else isHorizontalPathClear board (increaseFromInHorizontalPathClear from to) to

isVerticalPathClear :: Board -> Int -> Int -> Bool
isVerticalPathClear board from to = if from == to then isEmptyField (fields board !! from) else isEmptyField (fields board !! from) && (isVerticalPathClear board (increaseFromInVerticalPathClear from to) to)

isHorizontalPathClear :: Board -> Int -> Int -> Bool
isHorizontalPathClear board from to = if from == to then isEmptyField (fields board !! from) else isEmptyField (fields board !! from) && (isHorizontalPathClear board (increaseFromInHorizontalPathClear from to) to)

increaseFromInVerticalPathClear from to = (if from < to then from+boardSize else from-boardSize)

increaseFromInHorizontalPathClear from to = (if from < to then from+1 else from-1)

isNotDiagonal :: Int -> Int -> Bool
isNotDiagonal from to = isHorizontal from to || isVertical from to

printablePlayer :: Char -> [Char]
printablePlayer a = if a == 'O' then "Offense" else "Defense"

-- We're going to use recursion because that's just so much fancier than hardcoding 18 string positions
printableLines :: Int -> String -> String
printableLines prefix a = if length a > boardSize then "\n" ++ [chr prefix] ++ " |" ++ ((take boardSize a) ++ "|" ++ (printableLines (prefix-1) (drop boardSize a))) else ("\n" ++ [chr prefix] ++ " |" ++ a)

printableMap  :: String -> String
printableMap a = "  ___________" ++ printableLines (ord 'A' + boardSize - 1) a ++ "|\n  ‾‾‾‾‾‾‾‾‾‾‾" ++ "\n   " ++ ['1'..'9'] ++ "\n"

readableToInternalPosition :: String -> Int
readableToInternalPosition a = (boardSize - (ord (a !! 0) - (ord 'a')) - 1) * boardSize + (read [a !! 1] :: Int) - 1

invertNextPlayer :: Char -> Char
invertNextPlayer player = if player == 'O' then 'D' else 'O'

swapAtIndex :: Int -> String -> Char -> String
swapAtIndex from old swapWith = (fst (splitAt from old)) ++ [swapWith] ++ tail(snd (splitAt from old))

actuallyMove :: Board -> Int -> Int -> Board
actuallyMove board from to = Board (swapAtIndex from (swapAtIndex to (fields board) (fields board !! from)) ' ') (invertNextPlayer $ nextPlayer board)

canMove :: Board -> Int -> Int -> Bool
canMove board from to = isNotDiagonal from to && isPathClear board from to && isOwnerOfPiece (nextPlayer board) (fields board !! from) && (to /= throne)
  
validMove :: Int -> Int -> Bool
validMove from to = from >= 0 && to >= 0 && from <= boardSize * boardSize - 1 && to <= boardSize * boardSize - 1

internalMove :: Board -> Int -> Int -> Maybe Board
internalMove board from to = if (validMove from to) && (canMove board from to) then Just(actuallyMove board from to) else Nothing

readableMove :: Board -> String -> String -> Maybe Board
readableMove board from to = internalMove board (readableToInternalPosition from) (readableToInternalPosition to)

winner :: Board -> Maybe Char
winner board = Nothing