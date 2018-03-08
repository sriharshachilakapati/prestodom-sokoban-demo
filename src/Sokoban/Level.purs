module Sokoban.Level where

import Data.Array (concatMap, filter, foldl, length, zip, (!!), (..))
import Data.Foldable (maximum)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split, toCharArray)
import Data.Tuple (Tuple(Tuple))
import Prelude (append, map, not, show, ($), (*), (-), (<>), (==))
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (linearLayout, relativeLayout)
import PrestoDOM.Properties (background, height, margin, width)
import PrestoDOM.Types (Length(..))
import Sokoban.Types (Coord(..), GameState, Level, LevelRow, World, Entity)

level0 :: Level
level0 = createLevelData """
 ######
##    #
#   . ###
#   $   #
# .$@$. #
####$   #
   #.   #
   #   ##
   #####
"""

createWorld :: Level -> World
createWorld level =
  { soko: fromMaybe { x: 0, y: 0, w: 0, h: 0, c: "", t: '@' } $ (filter (\e -> e.t == '@') entities) !! 0
  , walls: filter (\e -> e.t == '#') entities
  , bags: filter (\e -> e.t == '$') entities
  , areas: filter (\e -> e.t == '.') entities
  , width: (levelWidth level) * 50
  , height: (levelHeight level) * 50
  }
  where
    grid :: Int -> Int -> Array Coord
    grid w h = concatMap (\y -> map (\x -> Coord x y) (0..(w - 1))) (0..(h - 1))

    entities :: Array Entity
    entities = createEntity `map` (filter (\(Tuple coord char) -> not (char == ' ')) $
                  zip (grid (levelWidth level) (levelHeight level)) (foldl append [] level))

createEntity :: Tuple Coord Char -> Entity
createEntity (Tuple (Coord x y) c) = { x: x * 50, y: y * 50, w: 50, h: 50, c: color, t: c }
  where
    color = case c of
      '#' -> "#888888"
      '.' -> "#60a3bc"
      '$' -> "#b71540"
      '@' -> "#e58e26"
      _ -> "error"

levelHeight :: Level -> Int
levelHeight level = length level

levelWidth :: Level -> Int
levelWidth level = fromMaybe 0 $ maximum (length `map` level)

createLevelData :: String -> Level
createLevelData level = padLevel $
    toCharArray `map` split (Pattern "\n") (replaceAll (Pattern "\r") (Replacement "") level)

padLevel :: Level -> Level
padLevel level = padLevelRow `map` level
  where
    padLevelRow :: LevelRow -> LevelRow
    padLevelRow r = append r $ (\_ -> ' ') `map` (0..(lengthDiff r))

    lengthDiff :: LevelRow -> Int
    lengthDiff r = (levelWidth level) - (length r)

renderLevel :: forall i p. GameState -> PrestoDOM i p
renderLevel state =
  relativeLayout
    [ width Match_Parent
    , height Match_Parent
    ]
    [ renderEntity state.world.soko
    , relativeLayout
        [ width Match_Parent
        , height Match_Parent
        ]
        (map renderEntity (state.world.walls `append` state.world.areas `append` state.world.bags))
    ]

renderEntity :: forall i p. Entity -> PrestoDOM i p
renderEntity entity =
  linearLayout
    [ width $ V entity.w
    , height $ V entity.h
    , background entity.c
    , margin $ (show entity.x) <> "," <> (show entity.y) <> ",0,0"
    ]
    []
