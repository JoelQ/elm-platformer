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
  }

initialModel : Model
initialModel =
  { width = 500, height = 500, character = Character.initialModel }

-- UPDATE

type alias Action = Character.Action

update : Action -> Model -> Model
update action model =
  { model | character <- Character.update action model.character }


-- VIEW

view : Model -> Element
view model =
  collage 200 200
    [ rect 200 200 |> filled green
    , Character.view model.character
    ]

-- SIGNALS

actions : Signal Action
actions =
  Signal.sampleOn (Time.fps 30) Character.actions

models : Signal Model
models =
  Signal.foldp update initialModel actions

main : Signal Element
main = Signal.map view models
