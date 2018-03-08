module Sokoban.Types where

-- | A type for the game state. Consists all the data in the game
type GameState =
  { world :: World
  }

-- | A type for the game world.
type World =
  { soko :: Entity
  , walls :: Array Entity
  , bags :: Array Entity
  , areas :: Array Entity
  , width :: Int
  , height :: Int
  }

-- | A type for the coordinate in the tile map.
data Coord = Coord Int Int

-- | A type for the Entity. Consists the boundary.
type Entity =
  { x :: Int
  , y :: Int
  , w :: Int
  , h :: Int
  , t :: Char
  , c :: String
  }

-- | A type for the game level. Denotes the entities as characters
type Level = Array LevelRow
type LevelRow = Array Char
