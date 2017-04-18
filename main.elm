import Html exposing (program, Html)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height, stroke, fill)
import AnimationFrame
import Time exposing (Time)
import Keyboard
import Array exposing (Array)
import Array.Extra
import Random
import Random.Extra


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

startPosition : Position
startPosition =
  { x = 10
  , y = 0
  }

type alias UnitsPerSecond = Float

speed : UnitsPerSecond
speed = 7

type alias Shape =
  { position: Position
  , shapeName: ShapeName
  , rotation: Rotation
  , timeSinceMove: Time
  }

type alias PlacedShapes =
  Array (Array Bool)



-- SHAPES

type ShapeName = Square | Line | L
type Rotation = Zero | Ninty | OneEighty | TwoSeventy

type alias Layout = List Position
type alias LayoutSquare = ( LayoutOffset, LayoutOffset )
type alias LayoutOffset = Int

getLayout : Shape -> Layout
getLayout shape =
  case shape.shapeName of
    Square ->
      square shape
    Line ->
      line shape
    L ->
      l shape

mapLayout : Shape -> List ( Int, Int ) -> Layout
mapLayout shape coords =
  coords
    |> rotateLayout shape.rotation
    |> List.map (\(x, y) -> (shape.position.x + x, shape.position.y + y ))
    |> coordTuplesToLayout

rotateLayout : Rotation -> List ( Int, Int ) -> List ( Int, Int )
rotateLayout rotation coords =
  case rotation of
    Zero ->
      List.map (\(x, y) -> (x, y)) coords
    Ninty ->
      List.map (\(x, y) -> (-y, x)) coords
    OneEighty ->
      List.map (\(x, y) -> (-x, -y)) coords
    TwoSeventy ->
      List.map (\(x, y) -> (y, -x)) coords

coordTuplesToLayout : List ( Int, Int ) -> Layout
coordTuplesToLayout coords =
  coords
    |> List.map (\(x, y) -> { x = x, y = y })

square : Shape -> Layout
square shape =
  [ ( 0, 0 )
  , ( 1, 0 )
  , ( 0, 1 )
  , ( 1, 1 )
  ]
  |> mapLayout shape

line : Shape -> Layout
line shape =
  [ ( -1, 0 )
  , ( 0, 0 )
  , ( 1, 0 )
  , ( 2, 0 )
  ]
  |> mapLayout shape

l : Shape -> Layout
l shape =
  [ ( 0, -1 )
  , ( 0, 0 )
  , ( 0, 1 )
  , ( 1, 1 )
  ]
  |> mapLayout shape

-- MODEL

type alias Model =
  { shape : Shape
  , placedShapes: PlacedShapes
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
  , randomShape
  )



-- MESSAGES

type Msg
  = NoOp
  | FrameMsg Time
  | KeyMsg Keyboard.KeyCode
  | UpdateLayout ShapeName



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

randomShape : Cmd Msg
randomShape =
  Random.Extra.sample [Square, Line, L]
    |> Random.map (Maybe.withDefault Square)
    |> Random.generate UpdateLayout

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
    , randomShape
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