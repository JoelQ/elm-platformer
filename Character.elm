module Character where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Keyboard
import Debug

-- MODEL

type alias Model =
  { vx : Float
  , vy: Float
  , x : Float
  , y : Float
  , width : Float
  , height : Float
  }

initialModel : Model
initialModel =
  { vx = 0
  , vy = 0
  , x = 0
  , y = 0
  , width = 50
  , height = 100
  }

-- UPDATE

type Action = Left | Right | Jump | Noop

update : Action -> Model -> Model
update action model =
  accelerate action model
    |> gravity
    |> applyMovement
    |> Debug.watch "model"

accelerate : Action -> Model -> Model
accelerate action model =
  case action of
    Left -> { model | vx <- (-50) }
    Right -> { model | vx <- 50 }
    Jump -> { model | vy <- 500 }
    Noop -> { model | vx <- 0 }

gravity : Model -> Model
gravity model =
  { model | vy <- model.vy - (0.033 * 980) }

applyMovement : Model -> Model
applyMovement model =
  { model
    | x <- model.x + (0.033 * model.vx)
    , y <- model.y + (0.033 * model.vy)
  }

-- VIEW

view : Model -> Form
view model =
  rect model.width model.height
    |> filled blue
    |> move (model.x, model.y)

-- Signals

walking : Signal Action
walking =
  Signal.map arrowToAction Keyboard.arrows

arrowToAction : { x : Int, y : Int } -> Action
arrowToAction arrow =
  case arrow.x of
    1 -> Right
    (-1) -> Left
    _ -> Noop

jumps : Signal Action
jumps =
  Signal.map (\jump -> if jump then Jump else Noop) Keyboard.space

actions : Signal Action
actions =
  Signal.merge walking jumps
