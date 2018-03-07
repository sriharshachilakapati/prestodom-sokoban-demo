module Sokoban.Level where

import Data.Array (length)
import Data.Foldable (maximum)
import Data.Maybe (fromMaybe)
import Data.String (Pattern(..), Replacement(..), replaceAll, split, toCharArray)
import Prelude (map, ($))
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (linearLayout)
import PrestoDOM.Properties (background, height, orientation, width)
import PrestoDOM.Types (Length(..))
import Sokoban.Types (GameState, Level, LevelRow)

level0 :: Level
level0 = createLevelData """
 ########
##      #
#   .   #
#   $   #
# .$@$. #
####$   #
   #.   #
   #   ##
   #####
"""

levelHeight :: Level -> Int
levelHeight level = length level

levelWidth :: Level -> Int
levelWidth level = fromMaybe 0 $ maximum (length `map` level)

renderLevel :: forall i p. GameState -> Level -> PrestoDOM i p
renderLevel _ level =
  linearLayout
    [ width Match_Parent
    , height Match_Parent
    , orientation "vertical"
    ]
    (map renderLevelRow level)

renderLevelRow :: forall i p. LevelRow -> PrestoDOM i p
renderLevelRow row =
  linearLayout
    [ width Match_Parent
    , height $ V 50
    , orientation "horizontal"
    ]
    (map renderRowTile row)

renderRowTile :: forall i p. Char -> PrestoDOM i p
renderRowTile = case _ of
  '#' -> renderBlock "#888888"
  '.' -> renderBlock "#60a3bc"
  '$' -> renderBlock "#b71540"
  '@' -> renderBlock "#e58e26"
  _ -> renderBlock "transparent"

renderBlock :: forall i p. String -> PrestoDOM i p
renderBlock colorCode =
  linearLayout
    [ width $ V 50
    , height $ V 50
    , background colorCode
    ]
    []

createLevelData :: String -> Level
createLevelData level = toCharArray `map` split (Pattern "\n") (replaceAll (Pattern "\r") (Replacement "") level)
