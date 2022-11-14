{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import Control.Applicative
import Data.List (intercalate)
import Data.List.Split (chunksOf)
import Data.Set (isSubsetOf)
import qualified Data.Set as Set
import Domain.Board (Board)
import qualified Domain.Board as Board
import Domain.Field
import qualified Domain.Field as Field
import Domain.GameResult (GameResult)
import qualified Domain.GameResult as GameResult
import Domain.Piece (Piece)
import qualified Domain.Piece as Piece
import Domain.State (OngoingState (..), State (..))
import qualified Domain.State as State
import Flow
import Util

choosePlayerPiece :: IO Piece
choosePlayerPiece = undefined

whichPieceGoesFirst :: IO Piece
whichPieceGoesFirst = undefined

programLoop :: State -> IO ()
programLoop = undefined

drawBoard :: Board -> IO ()
drawBoard board =
  Field.allFields
    |> Set.toList
    |> map (\field -> (Board.getField field board, fromEnum field))
    |> map
      ( \case
          (Just piece, _) -> show piece
          (Nothing, value) -> show value
      )
    |> chunksOf 3
    |> map (\fields -> " " ++ intercalate " ║ " fields ++ " ")
    |> intercalate "\n═══╬═══╬═══\n"
    |> putStrLn

step :: OngoingState -> IO State
step state@OngoingState {..} = do
  nextMove <- if State.isComputerTurn <| State.ongoing state then getComputerMove currentBoard else getPlayerMove currentBoard
  takeField state nextMove

getComputerMove :: Board -> IO Field
getComputerMove = undefined

getPlayerMove :: Board -> IO Field
getPlayerMove = undefined

takeField :: OngoingState -> Field -> IO State
takeField = undefined

getGameResult :: Board -> Maybe GameResult
getGameResult board =
  let crossWin = isWinner board Piece.X
      noughtWin = isWinner board Piece.O
   in case (crossWin, noughtWin) of
        (True, True) -> error "It should not be possible for both players to win!"
        (True, _) -> Just <| GameResult.Win Piece.X
        (_, True) -> Just <| GameResult.Win Piece.O
        _ -> Nothing

isWinner :: Board -> Piece -> Bool
isWinner board piece =
  let occupiedFields = Set.fromList <| Board.fieldsOccupiedByPiece piece board
   in any (\combinations -> Set.fromList combinations `isSubsetOf` occupiedFields) Board.winnerCombinations

main :: IO ()
main = undefined
