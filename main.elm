import AnimationFrame
import Time exposing (Time)
import Keyboard
import Array exposing (Array)
import Html exposing (program)

import View exposing (view)
import Messages exposing (Msg(NoOp, FrameMsg, KeyMsg, UpdatePiece))
import Model exposing
  ( Model
  , Rotation(Zero, Ninty, OneEighty, TwoSeventy)
  , square
  , startPosition
  , playArea
  , updateTimeSinceMove
  , tryRotate
  , collidesLeft
  , collidesRight
  , collidesBelow
  , mapPiece
  , updateShapeX
  , updateShapeY
  , addToPlacedPieces
  , newShape
  , startSpeed
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
  ( { activePiece =
      { position = startPosition
      , piece = square
      , rotation = Zero
      }
    , placedPieces =
      False
        |> Array.repeat playArea.height
        |> Array.repeat playArea.width
    , game =
      { speed = startSpeed
      , timeSinceMove = 0
      }
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
      let timeSinceMove = model.game.timeSinceMove + diff
      in
        if Time.inSeconds timeSinceMove >= 1 / model.game.speed then
          model
            |> updateTimeSinceMove (Time.inSeconds timeSinceMove - 1 / model.game.speed)
            |> moveDown
        else
          ( model |> updateTimeSinceMove timeSinceMove, Cmd.none )
    KeyMsg keyCode ->
      if keyCode == 37 then
        moveLeft model
      else if keyCode == 38 then
        rotatePiece model
      else if keyCode == 39 then
        moveRight model
      else if keyCode == 40 then
        moveDown model
      else
        ( model, Cmd.none )
    UpdatePiece piece ->
      ( { model | activePiece =
        { position = model.activePiece.position
        , piece = piece
        , rotation = model.activePiece.rotation
        }
      }, Cmd.none )

rotatePiece : Model -> ( Model, Cmd Msg )
rotatePiece model =
  case model.activePiece.rotation of
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
  if List.any ( collidesLeft model.placedPieces ) ( mapPiece model.activePiece ) then
    ( model, Cmd.none )
  else
    ( updateShapeX -1 model, Cmd.none )

moveRight : Model -> ( Model, Cmd Msg )
moveRight model =
  if List.any ( collidesRight model.placedPieces ) ( mapPiece model.activePiece ) then
    ( model, Cmd.none )
  else
    ( updateShapeX 1 model, Cmd.none )

moveDown : Model -> ( Model, Cmd Msg )
moveDown model =
  if List.any ( collidesBelow model.placedPieces ) ( mapPiece model.activePiece ) then
    ( model
      |> addToPlacedPieces
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