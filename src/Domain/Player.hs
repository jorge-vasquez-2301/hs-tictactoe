module Domain.Player
  ( Player (..),
  )
where

data Player = Computer | Human deriving (Eq)