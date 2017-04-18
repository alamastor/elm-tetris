import AnimationFrame
import Time exposing (Time)
import Keyboard
import Array exposing (Array)
import Array.Extra
import Html exposing (program)

import View exposing (view)
import Messages exposing (Msg(NoOp, FrameMsg, KeyMsg, UpdateLayout))
import Model exposing
  ( Model
  , ShapeName(Square)
  , Rotation(Zero, Ninty, OneEighty, TwoSeventy)
  , PlacedShapes
  , Position
  , Shape
  , getLayout
  , startPosition
  , speed
  , playArea
  )
import Commands


-- MAIN

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

init : ( Model, Cmd Msg )
init =
  ( { shape =
      { position = startPosition
      , shapeName = Square
      , rotation = Zero
      , timeSinceMove = 0
      }
    , placedShapes =
      False
        |> Array.repeat playArea.height
        |> Array.repeat playArea.width
    }
  , Commands.randomShape
  )


-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    FrameMsg diff ->
      let timeSinceMove = model.shape.timeSinceMove + diff
      in
        if Time.inSeconds timeSinceMove >= 1 / speed then
          model
            |> updateTimeSinceMove (Time.inSeconds timeSinceMove - 1 / speed)
            |> moveDown
        else
          ( model |> updateTimeSinceMove timeSinceMove, Cmd.none )
    KeyMsg keyCode ->
      if keyCode == 37 then
        moveLeft model
      else if keyCode == 38 then
        rotateShape model
      else if keyCode == 39 then
        moveRight model
      else if keyCode == 40 then
        moveDown model
      else
        ( model, Cmd.none )
    UpdateLayout shapeName ->
      ( { model | shape =
        { position = model.shape.position
        , shapeName = shapeName
        , rotation = model.shape.rotation
        , timeSinceMove = model.shape.timeSinceMove
        }
      }, Cmd.none )

rotateShape : Model -> ( Model, Cmd Msg )
rotateShape model =
  case model.shape.rotation of
    Zero ->
      ( setRotation Ninty model, Cmd.none )
    Ninty ->
      ( setRotation OneEighty model, Cmd.none )
    OneEighty ->
      ( setRotation TwoSeventy model, Cmd.none )
    TwoSeventy ->
      ( setRotation Zero model, Cmd.none )

setRotation : Rotation -> Model -> Model
setRotation rotation model =
  { model
    | shape =
    { position = model.shape.position
    , shapeName = model.shape.shapeName
    , rotation = rotation
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

moveLeft : Model -> ( Model, Cmd Msg )
moveLeft model =
  if List.any ( collidesLeft model.placedShapes ) ( getLayout model.shape ) then
    ( model, Cmd.none )
  else
    ( updateShapeX -1 model, Cmd.none )

moveRight : Model -> ( Model, Cmd Msg )
moveRight model =
  if List.any ( collidesRight model.placedShapes ) ( getLayout model.shape ) then
    ( model, Cmd.none )
  else
    ( updateShapeX 1 model, Cmd.none )

moveDown : Model -> ( Model, Cmd Msg )
moveDown model =
  if List.any ( collidesBelow model.placedShapes ) ( getLayout model.shape ) then
    ( model
      |> addToPlacedShapes
      |> newShape
      |> clearFullRows
    , Commands.randomShape
    )
  else
    ( updateShapeY 1 model, Cmd.none )

updateShapeX : Int -> Model -> Model
updateShapeX change model =
  { model | shape =
    { position = updatePositionX change model.shape.position
    , shapeName = model.shape.shapeName
    , rotation = model.shape.rotation
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

updatePositionX : Int -> Position -> Position
updatePositionX change position =
  { position | x = position.x + change}

updateShapeY : Int -> Model -> Model
updateShapeY change model =
  { model | shape =
    { position = updatePositionY change model.shape.position
    , shapeName = model.shape.shapeName
    , rotation = model.shape.rotation
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

updatePositionY : Int -> Position -> Position
updatePositionY change position =
  { position | y = position.y + change}

updateTimeSinceMove : Time -> Model -> Model
updateTimeSinceMove timeSinceMove model =
  { model | shape =
    { position = model.shape.position
    , shapeName = model.shape.shapeName
    , rotation = model.shape.rotation
    , timeSinceMove = timeSinceMove
    }
  }

collidesBelow : PlacedShapes -> Position -> Bool
collidesBelow placedShapes position =
  let
    belowPosition =
      { position | y = position.y + 1 }
  in
    if belowPosition.y >= playArea.height then
      True
    else if collidesPlaced belowPosition placedShapes then
      True
    else
      False

collidesLeft : PlacedShapes -> Position -> Bool
collidesLeft placedShapes position =
  let
    leftPosition =
      { position | x = position.x - 1 }
  in
    if leftPosition.x < 0 then
      True
    else if collidesPlaced leftPosition placedShapes then
      True
    else
      False

collidesRight : PlacedShapes -> Position -> Bool
collidesRight placedShapes position =
  let
    rightPosition = { position | x = position.x + 1 }
  in
    if rightPosition.x >= playArea.width then
      True
    else if collidesPlaced rightPosition placedShapes then
      True
    else
      False

getPlacedVal : Int -> Int -> PlacedShapes -> Bool
getPlacedVal x y placedShapes =
  placedShapes
    |> Array.get x
    |> Maybe.withDefault (False |> Array.repeat playArea.height)
    |> Array.get y
    |> Maybe.withDefault False

collidesPlaced : Position -> PlacedShapes -> Bool
collidesPlaced position placedShapes =
  placedShapes |> getPlacedVal position.x position.y

newShape : Model -> Model
newShape model =
  { model | shape =
    { position = startPosition
    , shapeName = model.shape.shapeName
    , rotation = model.shape.rotation
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

addToPlacedShapes : Model -> Model
addToPlacedShapes model =
  { model
    | placedShapes = placeLayout model.shape model.placedShapes
  }

placeLayout : Shape -> PlacedShapes -> PlacedShapes
placeLayout shape placedShapes =
  List.foldl (setPlaced True) placedShapes (getLayout shape)

setPlaced : Bool -> Position -> PlacedShapes -> PlacedShapes
setPlaced isSet position placedShapes =
  let
    column = Array.get position.x placedShapes
  in
    case column of
      Nothing ->
        placedShapes
      Just column ->
        Array.set position.x (Array.set position.y isSet column) placedShapes

clearFullRows : Model -> Model
clearFullRows model =
  let
    placedShapes = model.placedShapes
    completedRows = getCompletedRows placedShapes
  in
    { model
      | placedShapes = Array.map (\col -> clearIfRowComplete completedRows col) placedShapes
    }

getCompletedRows : PlacedShapes -> Array Bool
getCompletedRows placedShapes =
  let
    fullCol = Array.repeat playArea.height True
  in
    Array.foldl (\col prevCol -> bothTrue col prevCol ) fullCol placedShapes

clearIfRowComplete : Array Bool -> Array Bool -> Array Bool
clearIfRowComplete completedRows col =
  col
    |> Array.indexedMap (\y isSet -> (y, isSet))
    |> Array.Extra.removeWhen (\(y, isSet) -> rowIsComplete y completedRows)
    |> Array.map (\(_, isSet) -> isSet)
    |> Array.Extra.resizerRepeat playArea.height False

rowIsComplete : Int -> Array Bool -> Bool
rowIsComplete y completedRows =
  completedRows
    |> Array.get y
    |> Maybe.withDefault False

bothTrue : Array Bool -> Array Bool -> Array Bool
bothTrue array1 array2 =
  Array.Extra.map2 (&&) array1 array2



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs FrameMsg
    , Keyboard.downs KeyMsg
    ]