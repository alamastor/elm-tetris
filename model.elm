module Model exposing
  ( Model
  , ShapeName(..)
  , Rotation(..)
  , PlacedShapes
  , Position
  , Shape
  , playArea
  , startPosition
  , pixelsPerUnit
  , toSvgPix
  , getLayout
  , speed
  , tryRotate
  , updateTimeSinceMove
  , collidesLeft
  , collidesRight
  , collidesBelow
  , updateShapeX
  , updateShapeY
  , addToPlacedShapes
  , newShape
  , clearFullRows
  )

import Time exposing (Time)
import Array exposing (Array)
import Array.Extra
import Debug exposing (log)


type alias Model =
  { shape : Shape
  , placedShapes: PlacedShapes
  }

pixelsPerUnit : Int
pixelsPerUnit = 30

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
  { width = 13
  , height = 20
  }

type alias Position =
  { x: Unit
  , y: Unit
  }

startPosition : Position
startPosition =
  { x = 6
  , y = 0
  }

type alias UnitsPerSecond = Float

speed : UnitsPerSecond
speed = 5

type alias Shape =
  { position: Position
  , shapeName: ShapeName
  , rotation: Rotation
  , timeSinceMove: Time
  }

type alias PlacedShapes =
  Array (Array Bool)

type ShapeName = Square | Line | L
type Rotation = Zero | Ninty | OneEighty | TwoSeventy

type alias Layout = List Position

type alias LayoutSquare = ( LayoutOffset, LayoutOffset )
type alias LayoutOffset = Int

getLayout : Shape -> Layout
getLayout shape =
  case shape.shapeName of
    Square ->
      mapLayout square shape
    Line ->
      mapLayout line shape
    L ->
      mapLayout l shape

mapLayout : ShapeMap -> Shape -> Layout
mapLayout shapeMap shape =
  if shapeMap.rotates then
    shapeMap.layout
      |> rotateLayout shape.rotation
      |> List.map (\(x, y) -> (shape.position.x + x, shape.position.y + y ))
      |> coordTuplesToLayout
  else
    shapeMap.layout
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

coordTuplesToLayout : List ( Int, Int ) -> List Position
coordTuplesToLayout coords =
  coords
    |> List.map (\(x, y) -> { x = x, y = y })


type alias ShapeMap =
  { layout: List ( Int, Int )
  , rotates: Bool
  }

square : ShapeMap
square =
  { layout =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 0, 1 )
    , ( 1, 1 )
    ]
  , rotates = False
  }

line : ShapeMap
line =
  { layout =
    [ ( -1, 0 )
    , ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    ]
  , rotates = True
  }

l : ShapeMap
l =
  { layout =
    [ ( 0, -1 )
    , ( 0, 0 )
    , ( 0, 1 )
    , ( 1, 1 )
    ]
  , rotates = True
  }

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

tryRotate : Rotation -> Model -> Model
tryRotate rotation model =
  let
    rotated = setRotation rotation model
  in
    if shapeCollides rotated then
      if okToMoveRight rotated then
        log("ok to move right!")
        updateShapeX 1 rotated
      else if okToMoveLeft rotated then
        log("ok to move left!")
        updateShapeX -1 rotated
      else
        model
    else
      log("in bounds")
      rotated

outOfBounds : Position -> Bool
outOfBounds position =
  if
    position.x < 0 ||
    position.x >= playArea.width ||
    position.y >= playArea.height
  then
    True
  else
    False

collides : PlacedShapes -> Position -> Bool
collides placedShapes position =
  if collidesPlaced position placedShapes || outOfBounds position then
    True
  else
    False

shapeCollides : Model -> Bool
shapeCollides model =
  model.shape
    |> getLayout
    |> List.any (collides model.placedShapes)

okToMoveRight : Model -> Bool
okToMoveRight model =
  model
    |> updateShapeX 1
    |> shapeCollides
    |> not

okToMoveLeft : Model -> Bool
okToMoveLeft model =
  model
    |> updateShapeX -1
    |> shapeCollides
    |> not