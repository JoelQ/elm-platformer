module Collision where

type alias Subject =
  { vx : Float
  , vy: Float
  , x : Float
  , y : Float
  , width : Float
  , height : Float
  }

type alias Object =
  { x : Float
  , y : Float
  , width : Float
  , height : Float
  }

colliding : Subject -> Object -> Bool
colliding subject object =
  let nextSubject = applyMovement subject
      minimumCenterDistanceX = (object.width + nextSubject.width) / 2
      centerDistanceX = object.x - nextSubject.x
      minimumCenterDistanceY = (object.height + nextSubject.height) / 2
      centerDistanceY = object.y - nextSubject.y
      overlapX = (abs centerDistanceX) < minimumCenterDistanceX
      overlapY = (abs centerDistanceY) < minimumCenterDistanceY
      headingTowardsObstacleX = nextSubject.vx / (abs nextSubject.vx) == centerDistanceX / (abs centerDistanceX)
      headingTowardsObstacleY = nextSubject.vy / (abs nextSubject.vy) == centerDistanceY / (abs centerDistanceY)
  in
     overlapX && overlapY && (headingTowardsObstacleX || headingTowardsObstacleY)


headingTowardsObjectX :  Subject -> Object -> Bool
headingTowardsObjectX subject object =
  let nextSubject = applyMovement subject
      distanceNow = object.x - subject.x |> abs
      distanceNext = object.x - nextSubject.x |> abs
      minimumCenterDistanceY = (object.height + subject.height) / 2
      centerDistanceY = object.y - subject.y |> abs
      overlapY = centerDistanceY < minimumCenterDistanceY
  in
     distanceNext < distanceNow  && overlapY

headingTowardsObjectY :  Subject -> Object -> Bool
headingTowardsObjectY subject object =
  let nextSubject = applyMovement subject
      distanceNow = object.y - subject.y |> abs
      distanceNext = object.y - nextSubject.y |> abs
      minimumCenterDistanceX = (object.width + subject.width) / 2
      centerDistanceX = object.x - subject.x |> abs
      overlapX = centerDistanceX < minimumCenterDistanceX
  in
     distanceNext < distanceNow  && overlapX

applyMovement : Subject -> Subject
applyMovement model =
  { model
    | x <- model.x + (0.033 * model.vx)
    , y <- model.y + (0.033 * model.vy)
  }

