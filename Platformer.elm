import Graphics.Element exposing (..)
import Graphics.Collage exposing (..)
import Color exposing (..)
import Text exposing (..)
import Time
import Character
import Collision

-- MODEL

type alias Model =
  { width : Float
  , height : Float
  , character : Character.Model
  , platforms : List Platform
  , viewport : Viewport
  , goal : Goal
  , gameWon : Bool
  }

type alias Platform =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }

type alias Goal =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }

type alias Viewport =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }

initialModel : Model
initialModel =
  { width = 750
  , height = 500
  , viewport = { x = -125, y = 0, width = 500, height = 500 }
  , character = Character.initialModel
  , gameWon = False
  , goal = { x = 350, y = -250, width = 50, height = 100 }
  , platforms =
    [ {x = (-150), y = (-225), width = 200, height = 50}
    , {x = 0, y = -100, width = 200, height = 50}
    , {x = 150, y = 25, width = 200, height = 50}
    ]
  }

edges : Model -> List Platform
edges model =
  [
    { x = model.viewport.x
    , y = -(model.viewport.height/2 + 10)
    , width = model.viewport.width
    , height = 20
    }
  ]

-- UPDATE

type alias Action = Character.Action

update : Action -> Model -> Model
update action model =
  if model.gameWon then
     model
  else
    { model
    | character <- Character.update action (model.platforms ++ (edges model) ++ [model.goal]) model.character
    , viewport <- updateViewport model.character model.viewport
    , gameWon <- Collision.colliding model.character model.goal
    }

updateViewport : Character.Model -> Viewport -> Viewport
updateViewport character viewport =
  { viewport 
  | x <- if (abs (character.x - viewport.x)) > 100 then character.x else viewport.x
  }

-- VIEW

view : Model -> Element
view model =
  if model.gameWon then
     victory model
  else
    renderWorld model

renderWorld : Model -> Element
renderWorld model =
  let background = rect model.viewport.width model.viewport.height
        |> filled green
      character = model.character
        |> translateModelToViewport model.viewport
        |> Character.view
      platforms = model.platforms
        |> List.map (translatePlatformToViewport model.viewport)
        |> List.map viewPlatform
      goal = model.goal
        |> translateGoalToViewport model.viewport
        |> viewGoal
  in
    collage (round model.viewport.width) (round model.viewport.height)
    ([background, character, goal] ++ platforms)

translateModelToViewport : Viewport -> Character.Model -> Character.Model
translateModelToViewport viewport item =
  { item | x <- item.x - viewport.x }

translatePlatformToViewport : Viewport -> Platform -> Platform
translatePlatformToViewport viewport item =
  { item | x <- item.x - viewport.x }

translateGoalToViewport : Viewport -> Goal -> Goal
translateGoalToViewport viewport item =
  { item | x <- item.x - viewport.x }


viewPlatform : Platform -> Form
viewPlatform platform =
  rect platform.width platform.height
    |> filled brown
    |> move (platform.x, platform.y)

viewGoal : Goal -> Form
viewGoal goal =
  rect goal.width goal.height
    |> filled yellow
    |> move (goal.x, goal.y)

victory : Model -> Element
victory model =
  let
      fadedWorld = renderWorld model
        |> toForm
        |> alpha 0.5
      victoryText = fromString "Victory!"
        |> Text.height 40
        |> centered
        |> toForm
  in
   collage (round model.viewport.width) (round model.viewport.height)
   [fadedWorld, victoryText]


-- SIGNALS

actions : Signal Action
actions =
  Signal.sampleOn (Time.fps 30) Character.actions

models : Signal Model
models =
  Signal.foldp update initialModel actions

main : Signal Element
main = Signal.map view models
