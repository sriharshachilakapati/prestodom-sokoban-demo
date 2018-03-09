module Sokoban.Level where

import Data.Array (concatMap, filter, foldl, length, zip, (!!), (..))
import Data.Foldable (maximum)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split, toCharArray, trim)
import Data.Tuple (Tuple(Tuple))
import Prelude (append, map, not, show, ($), (*), (-), (<>), (==))
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (linearLayout, relativeLayout)
import PrestoDOM.Properties (background, cornerRadius, gravity, height, margin, padding, stroke, width)
import PrestoDOM.Types (Length(..))
import Sokoban.Types (Coord(..), GameState, Level, LevelRow, World, Entity)

createWorld :: Level -> World
createWorld level =
  { soko: fromMaybe { x: 0, y: 0, w: 0, h: 0, c: "", t: '@', nextPos: Coord 0 0 }
            $ (filter (\e -> e.t == '@') entities) !! 0
  , walls: filter (\e -> e.t == '#') entities
  , bags: filter (\e -> e.t == '$') entities
  , areas: filter (\e -> e.t == '.') entities
  , width: ((levelWidth level) - 1) * 50
  , height: (levelHeight level) * 50
  }
  where
    grid :: Int -> Int -> Array Coord
    grid w h = concatMap (\y -> map (\x -> Coord x y) (0..(w - 1))) (0..(h - 1))

    entities :: Array Entity
    entities = createEntities `concatMap` (filter (\(Tuple coord char) -> not (char == ' ')) $
                  zip (grid (levelWidth level) (levelHeight level)) (foldl append [] level))

createEntities :: Tuple Coord Char -> Array Entity
createEntities (Tuple coord c) =
  case c of
    '+' -> [ createEntity coord '@', createEntity coord '.' ]
    '*' -> [ createEntity coord '$', createEntity coord '.' ]
    _   -> [ createEntity coord c ]

createEntity :: Coord -> Char -> Entity
createEntity (Coord x y) c = { x: xPos, y: yPos, w: 50, h: 50, c: color, t: c, nextPos: Coord xPos yPos }
  where
    xPos = x * 50
    yPos = y * 50

    color = case c of
      '#' -> "#212020"
      '.' -> "#690000"
      '$' -> "#823b05"
      '@' -> "#e58e26"
      _ -> "error"

levelHeight :: Level -> Int
levelHeight level = length level

levelWidth :: Level -> Int
levelWidth level = fromMaybe 0 $ maximum (length `map` level)

createLevelData :: String -> Level
createLevelData level = padLevel $
    toCharArray `map` filter (\s -> not (trim s == ""))
        (split (Pattern "\n") (replaceAll (Pattern "\r") (Replacement "") level))

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
    , background "#10733a"
    , cornerRadius "10"
    , padding "25,25,0,0"
    ]
    [ relativeLayout
        [ width Match_Parent
        , height Match_Parent
        ]
        (map renderEntity (state.world.walls `append` state.world.areas `append` state.world.bags))
    , renderEntity state.world.soko
    ]

renderEntity :: forall i p. Entity -> PrestoDOM i p
renderEntity entity =
  linearLayout
    [ width $ V entity.w
    , height $ V entity.h
    , padding "1,1,1,1"
    , margin $ (show entity.x) <> "," <> (show entity.y) <> ",0,0"
    , gravity "center"
    ]
    [ linearLayout
        [ width $ if entity.t == '.' then V 20 else Match_Parent
        , height $ if entity.t == '.' then V 20 else Match_Parent
        , background entity.c
        , stroke "1,#777777"
        , cornerRadius "10"
        ]
        []
    ]
