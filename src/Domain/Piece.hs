module Domain.Piece
  ( Piece (..),
    next,
    make,
  )
where

import Data.Char (toUpper)

data Piece = X | O deriving (Eq, Show)

next :: Piece -> Piece
next X = O
next O = X

make :: String -> Maybe Piece
make value =
  case map toUpper value of
    "X" -> Just X
    "O" -> Just O
    _ -> Nothing