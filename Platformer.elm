import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Time
import Character

-- MODEL

type alias Model =
  { width : Float
  , height : Float
  , character : Character.Model
  , platforms : List Platform
  }

type alias Platform =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }

initialModel : Model
initialModel =
  { width = 500
  , height = 500
  , character = Character.initialModel
  , platforms = [ {x = 0, y = (-200), width = 200, height = 100} ]
  }

-- UPDATE

type alias Action = Character.Action

update : Action -> Model -> Model
update action model =
  { model | character <- Character.update action model.character }


-- VIEW

view : Model -> Element
view model =
  collage (round model.width) (round model.height)
    ([ rect model.width model.height |> filled green
    , Character.view model.character
    ] ++ List.map viewPlatform model.platforms)

viewPlatform : Platform -> Form
viewPlatform platform =
  rect platform.width platform.height
    |> filled brown
    |> move (platform.x, platform.y)

-- SIGNALS

actions : Signal Action
actions =
  Signal.sampleOn (Time.fps 30) Character.actions

models : Signal Model
models =
  Signal.foldp update initialModel actions

main : Signal Element
main = Signal.map view models
