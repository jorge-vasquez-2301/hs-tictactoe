module Domain.Field
  ( Field (..),
    make,
    allFields,
  )
where

import Data.Set (Set)
import qualified Data.Set as Set
import Flow

data Field
  = NorthWest
  | North
  | NorthEast
  | West
  | Center
  | East
  | SouthWest
  | South
  | SouthEast
  deriving (Eq, Ord, Enum)

make :: String -> Maybe Field
make "0" = Just NorthWest
make "1" = Just North
make "2" = Just NorthEast
make "3" = Just West
make "4" = Just Center
make "5" = Just East
make "6" = Just SouthWest
make "7" = Just South
make "8" = Just SouthEast
make _ = Nothing

allFields :: Set Field
allFields = Set.fromList <| enumFrom NorthWest