module Sokoban.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import Data.Array (filter, length, sort)
import Data.Int (ceil, floor, toNumber)
import FRP (FRP)
import FRP.Behavior.Keyboard (key)
import FRP.Event.Time (animationFrame)
import Prelude (Unit, bind, map, pure, unit, ($), (&&), (*), (*>), (+), (-), (<$>), (<*>), (<=), (==), (>), (>>>), (||))
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (linearLayout, relativeLayout)
import PrestoDOM.Properties (background, gravity, height, id_, orientation, width)
import PrestoDOM.Types (Length(..))
import PrestoDOM.Util (render)
import Sokoban.Level (createWorld, level0, renderLevel)
import Sokoban.Types (Coord(..), Direction(..), Entity, GameState)

-- | The function that is responsible to render the game screen. Checks the current screen and calls the respective
-- | render function, one specific to that screen.
renderGameScreen :: forall i p. GameState -> PrestoDOM i p
renderGameScreen state = renderLevel state

-- | The game world. Usually you'll notice the word widget, but this is named as world as it contains all the game
-- | entities. This is the template of the whole screen.
world :: forall i p. GameState -> PrestoDOM i p
world state =
  linearLayout
    [ id_ "container"
    , width Match_Parent
    , height Match_Parent
    , gravity "center"
    , orientation "vertical"
    , background "#2d3436"
    ]
    [ relativeLayout
        [ id_ "gameContainer"
        , width $ V state.world.width
        , height $ V state.world.height
        ]
        [ renderGameScreen state
        ]
    ]

-- | The entry point of the game. Here we initialize the state, create the entities, and starts rendering the game
main :: forall e. Eff (dom :: DOM, console :: CONSOLE, frp :: FRP | e) Unit
main = do
    let initialState = resetGame
    { stateBeh, updateState } <- render world initialState
    updateState (eval <$> key 37 <*> key 39 <*> key 38 <*> key 40 <*> stateBeh) animationFrame *>
    pure unit

-- | Central place to update the whole game
updateGame :: GameState -> GameState
updateGame state =
    if state.canMove then
      (updateSoko >>> updateBags >>> checkCanMove >>> checkWinCondition) state
    else
      (interpolateSoko >>> interpolateBags >>> checkCanMove) state
  where
    updateSoko s =
      if hasWall (getNextPos s.world.soko s.direction 1) then s
      else
        if hasBag (getNextPos s.world.soko s.direction 1) &&
           hasAny (getNextPos s.world.soko s.direction 2) then s
        else
          s { world = s.world { soko = moveEntity s.world.soko s.direction } }

    checkWinCondition s =
      if (sort $ entityToCoord `map` s.world.bags) == (sort $ entityToCoord `map` s.world.areas) then
        resetGame
      else s

    entityToCoord e = Coord e.x e.y

    hasWall coord = length (filter (\wall -> coord == Coord wall.x wall.y) state.world.walls) > 0
    hasBag coord = length (filter (\bag -> coord == Coord bag.x bag.y) state.world.bags) > 0
    hasAny coord = hasBag coord || hasWall coord

    updateBags s = s { world = s.world { bags = (updateBag s) `map` s.world.bags } }

    updateBag s bag =
      if s.world.soko.nextPos == Coord bag.x bag.y then
        moveEntity bag s.direction
      else bag

    interpolateSoko s = s { world = s.world { soko = interpolate s.world.soko } }
    interpolateBags s = s { world = s.world { bags = interpolate `map` s.world.bags } }
    checkCanMove s = s { canMove = s.world.soko.nextPos == Coord s.world.soko.x s.world.soko.y }

    moveEntity entity direction = entity { nextPos = getNextPos entity direction 1 }

getNextPos :: Entity -> Direction -> Int -> Coord
getNextPos entity direction steps =
  case direction of
    UP    -> Coord entity.x (entity.y - (50 * steps))
    LEFT  -> Coord (entity.x - (50 * steps)) entity.y
    DOWN  -> Coord entity.x (entity.y + (50 * steps))
    RIGHT -> Coord (entity.x + (50 * steps)) entity.y
    _     -> Coord entity.x entity.y

interpolate :: Entity -> Entity
interpolate entity@{ nextPos: (Coord x y) } =
  entity { x = calc entity.x x, y = calc entity.y y }
  where
    calc v0 v1 = roundFunc v0 v1 $ (1.0 - 0.1) * (toNumber v0) + 0.1 * (toNumber v1)
    roundFunc v0 v1 = if v0 <= v1 then ceil else floor

-- | Resets the game to the starting state. Creates the entities, and initializes the game state to the default.
resetGame :: GameState
resetGame =
  { world: createWorld level0
  , canMove: true
  , direction: NONE
  }

-- | The eval function is the function that gets called whenever a UI event occurred. In our case, the only event we
-- | are calling this is with is the animationFrame event which repeatedly occurs when in browser animation frame is
-- | granted for us. And yes, this uses `window.requestAnimationFrame` under the hood.
eval :: Boolean -> Boolean -> Boolean -> Boolean -> GameState -> GameState
eval keyLeft keyRight keyUp keyDown state =
    updateGame state { direction = getDirection (horizontalDir keyLeft keyRight) (verticalDir keyUp keyDown) }
  where
    getDirection x NONE = x
    getDirection NONE y = y
    getDirection _ _ = NONE

    horizontalDir true false = LEFT
    horizontalDir false true = RIGHT
    horizontalDir _ _ = NONE

    verticalDir true false = UP
    verticalDir false true = DOWN
    verticalDir _ _ = NONE
