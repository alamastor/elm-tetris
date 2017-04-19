import AnimationFrame
import Time exposing (Time)
import Keyboard
import Array exposing (Array)
import Html exposing (program)

import View exposing (view)
import Messages exposing (Msg(NoOp, FrameMsg, KeyMsg, UpdateLayout))
import Model exposing
  ( Model
  , ShapeName(Square)
  , Rotation(Zero, Ninty, OneEighty, TwoSeventy)
  , startPosition
  , playArea
  , speed
  , updateTimeSinceMove
  , tryRotate
  , collidesLeft
  , collidesRight
  , collidesBelow
  , getLayout
  , updateShapeX
  , updateShapeY
  , addToPlacedShapes
  , newShape
  , clearFullRows
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
      ( tryRotate Ninty model, Cmd.none )
    Ninty ->
      ( tryRotate OneEighty model, Cmd.none )
    OneEighty ->
      ( tryRotate TwoSeventy model, Cmd.none )
    TwoSeventy ->
      ( tryRotate Zero model, Cmd.none )

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


-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs FrameMsg
    , Keyboard.downs KeyMsg
    ]