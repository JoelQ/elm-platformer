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
update action bottom model =
  gravity model
    |> accelerate action
    |> preventCollisions bottom
    |> applyMovement
    -- |> Debug.watch "model"

accelerate : Action -> Model -> Model
accelerate action model =
  case action of
    Left -> { model | vx <- (-100) }
    Right -> { model | vx <- 100 }
    Jump -> { model | vy <- 500 }
    Noop -> { model | vx <- 0 }

gravity : Model -> Model
gravity model =
  { model | vy <- model.vy - (0.033 * 980) }

preventCollisions : List Entity -> Model -> Model
preventCollisions obstacles model =
  let collidingObstacles = List.filter (colliding model) obstacles |> Debug.watch "collisions"
  in
     List.foldl preventCollision model collidingObstacles

preventCollision : Entity -> Model -> Model
preventCollision obstacle model =
    { model
    | vx <- if headingTowardsObjectX model obstacle then 0 else model.vx
    , vy <- if headingTowardsObjectY model obstacle then 0 else model.vy
    , y <- if headingTowardsObjectY model obstacle then (obstacle.y + ((model.height + obstacle.height) / 2)) else model.y
    , x <- if headingTowardsObjectX model obstacle then (obstacle.x + ((model.width + obstacle.width) / 2)) else model.x
    }

colliding : Model -> Entity -> Bool
colliding model obstacle =
  let nextModel = applyMovement model
      minimumCenterDistanceX = (obstacle.width + nextModel.width) / 2
      centerDistanceX = obstacle.x - nextModel.x
      minimumCenterDistanceY = (obstacle.height + nextModel.height) / 2
      centerDistanceY = obstacle.y - nextModel.y
      overlapX = (abs centerDistanceX) < minimumCenterDistanceX
      overlapY = (abs centerDistanceY) < minimumCenterDistanceY
      headingTowardsObstacleX = nextModel.vx / (abs nextModel.vx) == centerDistanceX / (abs centerDistanceX)
      headingTowardsObstacleY = nextModel.vy / (abs nextModel.vy) == centerDistanceY / (abs centerDistanceY)
  in
     overlapX && overlapY && (headingTowardsObstacleX || headingTowardsObstacleY)

headingTowardsObjectX :  Model -> Entity -> Bool
headingTowardsObjectX model obstacle =
  let nextModel = applyMovement model
      distanceNow = obstacle.x - model.x |> abs
      distanceNext = obstacle.x - nextModel.x |> abs
      minimumCenterDistanceY = (obstacle.height + model.height) / 2
      centerDistanceY = obstacle.y - model.y |> abs
      overlapY = centerDistanceY < minimumCenterDistanceY
  in
     distanceNext < distanceNow  && overlapY |> Debug.watch "headingTowardsObjectX"

headingTowardsObjectY :  Model -> Entity -> Bool
headingTowardsObjectY model obstacle =
  let nextModel = applyMovement model
      distanceNow = obstacle.y - model.y |> abs
      distanceNext = obstacle.y - nextModel.y |> abs
      minimumCenterDistanceX = (obstacle.width + model.width) / 2
      centerDistanceX = obstacle.x - model.x |> abs
      overlapX = centerDistanceX < minimumCenterDistanceX
  in
     distanceNext < distanceNow  && overlapX |> Debug.watch "headingTowardsObjectY"

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
