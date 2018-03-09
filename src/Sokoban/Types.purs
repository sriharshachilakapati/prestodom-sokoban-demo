module Sokoban.Types where

import Prelude (class Eq, class Ord, compare, (&&), (==))

-- | A type for the game state. Consists all the data in the game
type GameState =
  { world :: World
  , direction :: Direction
  , canMove :: Boolean
  }

-- | An enumeration for direction
data Direction = UP | DOWN | LEFT | RIGHT | NONE

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
  , nextPos :: Coord
  }

-- | A type for the game level. Denotes the entities as characters
type Level = Array LevelRow
type LevelRow = Array Char

instance eqCoord :: Eq Coord where
  eq (Coord x1 y1) (Coord x2 y2) = x1 == x2 && y1 == y2

instance ordCoord :: Ord Coord where
  compare (Coord x1 y1) (Coord x2 y2) =
    if x1 == x2 then
      compare y1 y2
    else
      compare x1 x2
