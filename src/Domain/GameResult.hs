module Domain.GameResult
  ( GameResult (..),
    show,
  )
where

import Domain.Piece (Piece (O, X))
import Prelude hiding (show)

data GameResult = Win Piece | Draw

show :: GameResult -> String
show (Win X) = "Cross wins!"
show (Win O) = "Nought wins!"
show Draw = "It's a draw!"