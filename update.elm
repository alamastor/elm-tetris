module Update exposing (update, init)

import Time exposing (Time)
import Array exposing (Array)

import Model exposing
  ( Model
  , Rotation(Zero, Ninty, OneEighty, TwoSeventy)
  , GameState(Active, Paused, GameOver)
  , square
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
  , newPiece
  , startSpeed
  , clearFullRows
  , updateSpeed
  , checkGameOver
  , pieceMaxes
  )
import Messages exposing (Msg(NoOp, FrameMsg, KeyMsg, UpdateBothPieces, UpdateNextPiece))
import Commands

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    FrameMsg diff ->
      let timeSinceMove = model.game.timeSinceMove + diff
      in
        case model.game.gameState of
          Active ->
            if Time.inSeconds timeSinceMove >= 1 / model.game.speed then
              model
                |> updateTimeSinceMove (Time.inSeconds timeSinceMove - 1 / model.game.speed)
                |> updateSpeed 0.005
                |> moveDown
            else
              ( model |> updateTimeSinceMove timeSinceMove, Cmd.none )
          _ ->
            ( model, Cmd.none )
    KeyMsg keyCode ->
      case model.game.gameState of
        Active ->
          case keyCode of
            32 ->
              switchPaused model
            37 ->
              moveLeft model
            38 ->
              rotatePiece model
            39 ->
              moveRight model
            40 ->
              moveDown model
            _ ->
              ( model, Cmd.none )
        _ ->
          ( model, Cmd.none )

    UpdateNextPiece piece ->
      ( { model | nextPiece = piece } , Cmd.none )
    UpdateBothPieces (piece, nextPiece) ->
      let activePiece = model.activePiece
      in
        ( { model | activePiece = { activePiece | piece = piece }}
        , Cmd.none
        )

init : ( Model, Cmd Msg )
init =
  ( { activePiece = 
      { position =
        { x = round ((toFloat playArea.width) / 2) - 1
        , y = (\(_, maxY) -> -maxY) (pieceMaxes square) - 1
        }
        , piece = square
        , rotation = Zero
      }
    , nextPiece = square
    , placedPieces =
      Nothing
        |> Array.repeat playArea.height
        |> Array.repeat playArea.width
    , game =
      { speed = startSpeed
      , timeSinceMove = 0
      , gameState = Active
      }
    }
  , Commands.randomPiecePair
  )

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
      |> clearFullRows
      |> checkGameOver
      |> newPiece
    , Commands.randomNextPiece
    )
  else
    ( updateShapeY 1 model, Cmd.none )

switchPaused : Model -> ( Model, Cmd Msg )
switchPaused model =
  let game = model.game
  in
    case game.gameState of
      Active ->
        ( { model | game = { game | gameState = Paused } }
        , Cmd.none
        )
      Paused ->
        ( { model | game = { game | gameState = Active } }
        , Cmd.none
        )
      _ ->
        ( model, Cmd.none )