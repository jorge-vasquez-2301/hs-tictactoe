{-# LANGUAGE RecordWildCards #-}

module Domain.Board
  ( Board,
    fieldIsNotFree,
    fieldsOccupiedByPiece,
    isFull,
    unoccupiedFields,
    updated,
    empty,
    winnerCombinations,
    getField,
  )
where

import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Set (difference)
import qualified Data.Set as Set
import Domain.Field (Field)
import qualified Domain.Field as Field
import Domain.Piece (Piece)
import Flow

newtype Board = Board {fields :: Map Field Piece}

empty :: Board
empty = Board Map.empty

winnerCombinations :: [[Field]]
winnerCombinations =
  let horizontalWins = [[0, 1, 2], [3, 4, 5], [6, 7, 8]]
      verticalWins = [[0, 3, 6], [1, 4, 7], [2, 5, 8]]
      diagonalWins = [[0, 4, 8], [2, 4, 6]]
   in (horizontalWins ++ verticalWins ++ diagonalWins) |> map intsToFields
  where
    intsToFields :: [Integer] -> [Field]
    intsToFields ints =
      ints
        |> map show
        |> map Field.make
        |> map fromJust

fieldIsNotFree :: Field -> Board -> Bool
fieldIsNotFree field (Board {..}) = Map.member field fields

fieldsOccupiedByPiece :: Piece -> Board -> [Field]
fieldsOccupiedByPiece piece (Board {..}) =
  fields
    |> Map.toList
    |> filter (\(_, p) -> piece == p)
    |> map fst

isFull :: Board -> Bool
isFull (Board {..}) = Map.size fields == 9

unoccupiedFields :: Board -> [Field]
unoccupiedFields (Board {..}) =
  fields
    |> Map.keysSet
    |> difference Field.allFields
    |> Set.toList
    |> sort

updated :: Field -> Piece -> Board -> Board
updated field piece (Board {..}) = Board <| Map.insert field piece fields

getField :: Field -> Board -> Maybe Piece
getField field (Board {..}) = Map.lookup field fields
