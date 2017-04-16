import Html exposing (program, Html)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height, stroke, fill)
import AnimationFrame
import Time exposing (Time)
import Keyboard
import Array exposing (Array)


-- MAIN

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- BOARD

pixelsPerUnit : Int
pixelsPerUnit = 20

type alias Unit = Int

toSvgPix : Unit -> String
toSvgPix units =
  units * pixelsPerUnit |> toString

type alias Rectangle =
  { width: Unit
  , height: Unit
  }

playArea : Rectangle
playArea =
  { width = 21
  , height = 30
  }

type alias Position =
  { x: Unit
  , y: Unit
  }

type alias UnitsPerSecond = Float

speed : UnitsPerSecond
speed = 4

type alias Shape =
  { position: Position
  , timeSinceMove: Time
  }

type alias PlacedShapes =
  Array (Array Bool)



-- MODEL

type alias Model =
  { shape : Shape
  , placedShapes: PlacedShapes
  }

init : ( Model, Cmd Msg )
init =
  ( { shape =
      { position =
        { x = 10
        , y = 0
        }
      , timeSinceMove = 0
      }
    , placedShapes =
      False
        |> Array.repeat playArea.height
        |> Array.repeat playArea.width
    }
  , Cmd.none
  )



-- MESSAGES

type Msg
  = NoOp
  | FrameMsg Time
  | KeyMsg Keyboard.KeyCode



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
          ( model
            |> updateTimeSinceMove (Time.inSeconds timeSinceMove - 1 / speed)
            |> moveDown
          , Cmd.none
          )
        else
          ( model |> updateTimeSinceMove timeSinceMove, Cmd.none )
    KeyMsg keyCode ->
      if keyCode == 37 then
        ( moveLeft model, Cmd.none )
      else if keyCode == 39 then
        ( model |> moveRight, Cmd.none )
      else if keyCode == 40 then
        ( model |> moveDown, Cmd.none )
      else
        ( model, Cmd.none )

moveLeft : Model -> Model
moveLeft model =
  if collidesLeft model.shape model.placedShapes then
    model
  else
    updateShapeX -1 model

moveRight : Model -> Model
moveRight model =
  if collidesRight model.shape model.placedShapes then
    model
  else
    updateShapeX 1 model

moveDown : Model -> Model
moveDown model =
  if collidesBelow model.shape model.placedShapes then
    model
      |> addToPlacedShapes
      |> newShape
  else
    updateShapeY 1 model

updateShapeX : Int -> Model -> Model
updateShapeX change model =
  { model | shape =
    { position = updatePositionX change model.shape.position
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
    , timeSinceMove = timeSinceMove
    }
  }

collidesBelow : Shape -> PlacedShapes -> Bool
collidesBelow shape placedShapes =
  let
    position = shape.position
    belowPosition =
      { position | y = shape.position.y + 1 }
  in
    if belowPosition.y >= playArea.height then
      True
    else if collidesPlaced belowPosition placedShapes then
      True
    else
      False

collidesLeft : Shape -> PlacedShapes -> Bool
collidesLeft shape placedShapes =
  let
    position = shape.position
    leftPosition =
      { position | x = shape.position.x - 1 }
  in
    if leftPosition.x < 0 then
      True
    else if collidesPlaced leftPosition placedShapes then
      True
    else
      False

collidesRight : Shape -> PlacedShapes -> Bool
collidesRight shape placedShapes =
  let
    position = shape.position
    rightPosition =
      { position | x = shape.position.x + 1 }
  in
    if rightPosition.x < 0 then
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
    { position =
      { x = 10
      , y = 0
      }
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

addToPlacedShapes : Model -> Model
addToPlacedShapes model =
  { model
    | placedShapes = setPlaced model.shape.position model.placedShapes
  }

setPlaced : Position -> PlacedShapes -> PlacedShapes
setPlaced position placedShapes =
  let
    column = Array.get position.x placedShapes
  in
    case column of
      Nothing ->
        placedShapes
      Just column ->
        Array.set position.x (Array.set position.y True column) placedShapes



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs FrameMsg
    , Keyboard.downs KeyMsg
    ]



-- VIEW

placedShapesRects : PlacedShapes -> List (Svg Msg)
placedShapesRects placedShapes =
  placedShapes
    |> Array.indexedMap getIndexPair
    |> Array.toList
    |> List.concat
    |> List.map (\(xIdx, yIdx, isSet) -> (rect
      [ toSvgPix xIdx |> x
      , toSvgPix yIdx |> y
      , toSvgPix 1 |> width
      , toSvgPix 1 |> height
      , stroke "grey"
      , (\isSet -> if isSet then "green" else "none") isSet |> fill
      ] []))



getIndexPair : Int -> Array Bool -> List (Int, Int, Bool)
getIndexPair xIdx array =
  array
    |> Array.indexedMap (\yIdx isSet -> (xIdx, yIdx, isSet))
    |> Array.toList

view : Model -> Html Msg
view model =
  svg [ toSvgPix playArea.width |> width, toSvgPix playArea.height |> height ]
    ( List.append
      [ rect [ toSvgPix playArea.width |> width, toSvgPix playArea.height |> height, stroke "black", fill "none" ] []
      , rect
        [ toSvgPix model.shape.position.x |> x
        , toSvgPix model.shape.position.y |> y
        , toSvgPix 1 |> width
        , toSvgPix 1 |> height
        ] []
      ] ( placedShapesRects model.placedShapes ))