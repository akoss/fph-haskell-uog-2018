{- |

In this module you should define your game's logic and expose that logic through a custom API.

-}

module Tafl.Logic
  (
    printableMap
  , printablePlayer
  , readableMove
  , performAllCapture
  ) where

import Tafl.Core
import Data.List
import Data.Maybe
import Data.Char

boardSize = 9
throne = 40

-- Checking for capture

fieldOnLeft :: Int -> Maybe Int
fieldOnLeft field = if (field `mod` boardSize) == 0 then Nothing else Just (field - 1)

fieldOnRight :: Int -> Maybe Int 
fieldOnRight field = if (field `mod` boardSize) == (boardSize - 1) then Nothing else Just(field + 1)

fieldOnBottom :: Int -> Maybe Int
fieldOnBottom field = if (field + boardSize) >= (boardSize * boardSize) then Nothing else Just(field + boardSize)

fieldOnTop :: Int -> Maybe Int
fieldOnTop field = if (field - boardSize) < 0 then Nothing else Just(field - boardSize)

canCapture :: Maybe Char -> Maybe Char -> Bool
canCapture field byWhom = 
    case field of 
        Nothing -> False
        Just field -> case byWhom of
            Nothing -> False
            Just byWhom -> case field of 
                'O' -> byWhom == 'G' || byWhom == 'L' || byWhom == 'T'
                'L' -> byWhom == 'O' || byWhom == ':'
                'G' -> byWhom == 'O' || byWhom == ':'
                _   -> False

isCapturedFromFourSides :: Board -> Int -> Bool
isCapturedFromFourSides board field = 
       canCapture (valueOfField board (Just field)) (valueOfField board (fieldOnLeft field)) 
    && canCapture (valueOfField board (Just field)) (valueOfField board (fieldOnRight field))
    && canCapture (valueOfField board (Just field)) (valueOfField board (fieldOnTop field))
    && canCapture (valueOfField board (Just field)) (valueOfField board (fieldOnBottom field))

isCapturedFromTwoSides :: Board -> Int -> Bool
isCapturedFromTwoSides board field = 
    (  canCapture (valueOfField board (Just field)) (valueOfField board (fieldOnLeft field)) 
    && canCapture (valueOfField board (Just field)) (valueOfField board (fieldOnRight field))
    ) || (
       canCapture (valueOfField board (Just field)) (valueOfField board (fieldOnTop field))
    && canCapture (valueOfField board (Just field)) (valueOfField board (fieldOnBottom field))
    )

isKingNearThrone :: Board -> Int -> Bool
isKingNearThrone board field = valueOfField board (Just field) == (Just 'L') && (field == throne || field == throne + boardSize || field == throne - boardSize || field == throne - 1 || field == throne + 1)

isCaptured :: Board -> Int -> Bool
isCaptured board field = (isCapturedFromTwoSides board field) || ((isKingNearThrone board field) && isCapturedFromFourSides board field)

performActualCapture :: Board -> Int -> Board
performActualCapture board field = if isCaptured board field then (Board (swapAtIndex field (fields board) ' ') (nextPlayer board)) else board

performCapture :: Board -> Maybe Int -> Board
performCapture board field = 
  case field of 
    Nothing -> board
    Just field -> performActualCapture board field

performAllCapture :: Board -> Int -> Board
performAllCapture board field = performCapture (performCapture (performCapture (performCapture board (fieldOnLeft field)) (fieldOnRight field)) (fieldOnTop field)) (fieldOnBottom field)

valueOfField :: Board -> Maybe Int -> Maybe Char
valueOfField board field = 
    case field of
        Nothing -> Nothing
        Just field -> Just(fields board !! field)

-- Validating moves 

isOwnerOfPiece :: Char -> Char -> Bool
isOwnerOfPiece player piece = if player == 'D' then piece `elem` ['L','G'] else piece == 'O'

isEmptyField :: Char -> Bool
isEmptyField field = field == ' '

isVertical :: Int -> Int -> Bool
isVertical from to = (from `mod` boardSize) == (to `mod` boardSize)

isHorizontal :: Int -> Int -> Bool
isHorizontal from to = to >= (from - (from `mod` boardSize)) && to < (from - (from `mod` boardSize) + boardSize)

isNotDiagonal :: Int -> Int -> Bool
isNotDiagonal from to = isHorizontal from to || isVertical from to

isPathClear :: Board -> Int -> Int -> Bool
isPathClear board from to = if isVertical from to then isVerticalPathClear board (increaseFromInVerticalPathClear from to) to else isHorizontalPathClear board (increaseFromInHorizontalPathClear from to) to

isVerticalPathClear :: Board -> Int -> Int -> Bool
isVerticalPathClear board from to = if from == to then isEmptyField (fields board !! from) else isEmptyField (fields board !! from) && (isVerticalPathClear board (increaseFromInVerticalPathClear from to) to)

isHorizontalPathClear :: Board -> Int -> Int -> Bool
isHorizontalPathClear board from to = if from == to then isEmptyField (fields board !! from) else isEmptyField (fields board !! from) && (isHorizontalPathClear board (increaseFromInHorizontalPathClear from to) to)

increaseFromInVerticalPathClear from to = (if from < to then from+boardSize else from-boardSize)
increaseFromInHorizontalPathClear from to = (if from < to then from+1 else from-1)

-- Printing

printablePlayer :: Char -> [Char]
printablePlayer a = if a == 'O' then "Offense" else "Defense"
   -- We're going to use recursion because that's just so much fancier than hardcoding 18 string positions
printableLines :: Int -> String -> String
printableLines prefix a = if length a > boardSize then "\n" ++ [chr prefix] ++ " |" ++ ((take boardSize a) ++ "|" ++ (printableLines (prefix-1) (drop boardSize a))) else ("\n" ++ [chr prefix] ++ " |" ++ a)

printableMap  :: String -> String
printableMap a = "  ___________" ++ printableLines (ord 'A' + boardSize - 1) a ++ "|\n  ‾‾‾‾‾‾‾‾‾‾‾" ++ "\n   " ++ ['1'..'9'] ++ "\n"

readableToInternalPosition :: String -> Int
readableToInternalPosition a = (boardSize - (ord (a !! 0) - (ord 'a')) - 1) * boardSize + (read [a !! 1] :: Int) - 1

-- Helpers

invertNextPlayer :: Char -> Char
invertNextPlayer player = if player == 'O' then 'D' else 'O'

swapAtIndex :: Int -> String -> Char -> String
swapAtIndex from old swapWith = (fst (splitAt from old)) ++ [swapWith] ++ tail(snd (splitAt from old))

-- Movement

actuallyMove :: Board -> Int -> Int -> Board
actuallyMove board from to = Board (swapAtIndex from (swapAtIndex to (fields board) (fields board !! from)) (if from == 40 then ':' else ' ')) (invertNextPlayer $ nextPlayer board)

canMove :: Board -> Int -> Int -> Bool
canMove board from to = isNotDiagonal from to && isPathClear board from to && isOwnerOfPiece (nextPlayer board) (fields board !! from) && (to /= throne)
  
validMove :: Int -> Int -> Bool
validMove from to = from >= 0 && to >= 0 && from <= boardSize * boardSize - 1 && to <= boardSize * boardSize - 1

internalMove :: Board -> Int -> Int -> Maybe Board
internalMove board from to = if (validMove from to) && (canMove board from to) then Just (performAllCapture (actuallyMove board from to) to) else Nothing

readableMove :: Board -> String -> String -> Maybe Board
readableMove board from to = internalMove board (readableToInternalPosition from) (readableToInternalPosition to)

winner :: Board -> Maybe Char
winner board = Nothing