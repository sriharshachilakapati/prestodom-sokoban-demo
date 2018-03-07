module Sokoban.Types where

-- | A type for the game state. Consists all the data in the game
type GameState =
  {
  }

-- | A type for the game level. Denotes the entities as characters
type Level = Array LevelRow
type LevelRow = Array Char
