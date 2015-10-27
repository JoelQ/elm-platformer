module Character where

import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Keyboard
import Debug
import Collision

-- MODEL

type alias Model =
  { vx : Float
  , vy: Float
  , x : Float
  , y : Float
  , width : Float
  , height : Float
  }

type alias Entity =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }

initialModel : Model
initialModel =
  { vx = 0
  , vy = 0
  , x = 0
  , y = 150
  , width = 50
  , height = 100
  }

-- UPDATE

type Action = Left | Right | Jump | Noop

update : Action -> List Entity -> Model -> Model
update action platforms model =
  gravity model
    |> accelerate action platforms
    |> preventCollisions platforms
    |> Collision.applyMovement
    |> Debug.watch "model"

accelerate : Action -> List Entity -> Model -> Model
accelerate action platforms model =
  case action of
    Left -> { model | vx <- (-100) }
    Right -> { model | vx <- 100 }
    Jump -> { model | vy <- if List.any (Collision.colliding model) platforms then 500 else model.vy }
    Noop -> { model | vx <- 0 }

gravity : Model -> Model
gravity model =
  { model | vy <- model.vy - (0.033 * 980) }

preventCollisions : List Entity -> Model -> Model
preventCollisions obstacles model =
  let
      collidingObstacles = List.filter (Collision.colliding model) obstacles |> Debug.watch "collisions"
  in
     List.foldl preventCollision model collidingObstacles

preventCollision : Entity -> Model -> Model
preventCollision obstacle model =
  let
      vx = if Collision.headingTowardsObjectX model obstacle then 0 else model.vx
      vy = if Collision.headingTowardsObjectY model obstacle then 0 else model.vy
      yDistance = (model.height + obstacle.height) / 2
      yDirection = (model.y - obstacle.y) / (abs (model.y - obstacle.y))
      y = if Collision.headingTowardsObjectY model obstacle then (obstacle.y + yDistance * yDirection) else model.y
      xDistance = (model.width + obstacle.width) / 2
      xDirection = (model.x - obstacle.x) / (abs (model.x - obstacle.x))
      x = if Collision.headingTowardsObjectX model obstacle then (obstacle.x + xDistance * xDirection) else model.x
  in
    { model | vx <- vx, vy <- vy, y <- y, x <- x }

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
