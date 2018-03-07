module Sokoban.Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import DOM (DOM)
import FRP (FRP)
import FRP.Event.Time (animationFrame)
import Prelude (Unit, bind, pure, unit, (*>), (<$>))
import PrestoDOM.Core (PrestoDOM)
import PrestoDOM.Elements (imageView, linearLayout, relativeLayout)
import PrestoDOM.Properties (background, gravity, height, id_, imageUrl, orientation, width)
import PrestoDOM.Types (Length(..))
import PrestoDOM.Util (render)
import Sokoban.Types (GameState)

-- | The function that is responsible to render the game screen. Checks the current screen and calls the respective
-- | render function, one specific to that screen.
renderGameScreen :: forall i p. GameState -> PrestoDOM i p
renderGameScreen state = linearLayout [] []

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
        [ id_ "contentScreen"
        , width (V 640)
        , height (V 480)
        ]
        [ imageView
            [ id_ "background"
            , width Match_Parent
            , height Match_Parent
            , imageUrl "resources/background"
            ]
        , renderGameScreen state
        ]
    ]

-- | The entry point of the game. Here we initialize the state, create the entities, and starts rendering the game
main :: forall e. Eff (dom :: DOM, console :: CONSOLE, frp :: FRP | e) Unit
main = do
    let initialState = resetGame
    { stateBeh, updateState } <- render world initialState
    updateState (eval <$> stateBeh) animationFrame *>
    pure unit

-- | Central place to update the whole game
updateGame :: GameState -> GameState
updateGame state = state

-- | Resets the game to the starting state. Creates the entities, and initializes the game state to the default.
resetGame :: GameState
resetGame =
  {
  }

-- | The eval function is the function that gets called whenever a UI event occurred. In our case, the only event we
-- | are calling this is with is the animationFrame event which repeatedly occurs when in browser animation frame is
-- | granted for us. And yes, this uses `window.requestAnimationFrame` under the hood.
eval :: GameState -> GameState
eval state =
  updateGame state
