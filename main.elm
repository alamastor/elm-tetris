import Html exposing (program, Html)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height, stroke, fill)
import AnimationFrame
import Time exposing (Time)
import Keyboard
import Array exposing (Array)
import Debug exposing (log)


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
  , shapeName: ShapeName
  , timeSinceMove: Time
  }

type alias PlacedShapes =
  Array (Array Bool)



-- SHAPES

type ShapeName = Square | Line | L

type alias Layout = List Position
type alias LayoutSquare = ( LayoutOffset, LayoutOffset )
type alias LayoutOffset = Int

getLayout : Shape -> Layout
getLayout shape =
  case shape.shapeName of
    Square ->
      square shape.position
    Line ->
      line shape.position
    L ->
      line shape.position

mapLayout : Position -> List ( Int, Int ) -> Layout
mapLayout position coords =
  coords |> List.map (\(x, y) -> { x= position.x + x, y = position.y + y })

square : Position -> Layout
square position =
  [ ( 0, 0 )
  , ( 1, 0 )
  , ( 0, 1 )
  , ( 1, 1 )
  ]
  |> mapLayout position

line : Position -> Layout
line position =
  [ ( -1, 0 )
  , ( 0, 0 )
  , ( 1, 0 )
  , ( 2, 0 )
  ]
  |> mapLayout position

l : Position -> Layout
l position =
  [ ( 0, -1 )
  , ( 0, 0 )
  , ( 0, 1 )
  , ( 1, 1 )
  ]
  |> mapLayout position

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
      , shapeName = Square
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
  if List.any ( collidesLeft model.placedShapes ) ( getLayout model.shape ) then
    model
  else
    updateShapeX -1 model

moveRight : Model -> Model
moveRight model =
  if List.any ( collidesRight model.placedShapes ) ( getLayout model.shape ) then
    model
  else
    updateShapeX 1 model

moveDown : Model -> Model
moveDown model =
  if List.any ( collidesBelow model.placedShapes ) ( getLayout model.shape ) then
    model
      |> addToPlacedShapes
      |> newShape
  else
    updateShapeY 1 model

updateShapeX : Int -> Model -> Model
updateShapeX change model =
  { model | shape =
    { position = updatePositionX change model.shape.position
    , shapeName = model.shape.shapeName
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
    { position =
      { x = 10
      , y = 0
      }
    , shapeName = model.shape.shapeName
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
  List.foldl setPlaced placedShapes (square shape.position)

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

shapeRects : Shape -> List (Svg Msg)
shapeRects shape =
  List.map layoutRect (getLayout shape)

layoutRect : Position -> Svg Msg
layoutRect position =
  rect
    [ toSvgPix position.x |> Svg.Attributes.x
    , toSvgPix position.y |> Svg.Attributes.y
    , toSvgPix 1 |> width
    , toSvgPix 1 |> height
    ] []

placedShapesRects : PlacedShapes -> List(Svg Msg)
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
    ( List.concat
      [ [ rect [ toSvgPix playArea.width |> width, toSvgPix playArea.height |> height, stroke "black", fill "none" ] [] ]
      , shapeRects model.shape
      , ( placedShapesRects model.placedShapes )
      ]
    )