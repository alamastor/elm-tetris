module Model exposing
  ( Model
  , Rotation(..)
  , PlacedPieces
  , Position
  , Shape
  , Piece
  , square
  , line
  , l
  , Color
  , playArea
  , startPosition
  , pixelsPerUnit
  , toSvgPix
  , mapPiece
  , speed
  , tryRotate
  , updateTimeSinceMove
  , collidesLeft
  , collidesRight
  , collidesBelow
  , updateShapeX
  , updateShapeY
  , addToPlacedPieces
  , newShape
  , clearFullRows
  )

import Time exposing (Time)
import Array exposing (Array)
import Array.Extra


type alias Model =
  { shape : Shape
  , placedPieces: PlacedPieces
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
  , piece: Piece
  , rotation: Rotation
  , timeSinceMove: Time
  }

type alias PlacedPieces =
  Array (Array Bool)

type Rotation = Zero | Ninty | OneEighty | TwoSeventy

type alias Layout = List Position

type alias LayoutSquare = ( LayoutOffset, LayoutOffset )
type alias LayoutOffset = Int

mapPiece : Shape -> Layout
mapPiece shape =
  if shape.piece.rotates then
    shape.piece.layout
      |> rotateLayout shape.rotation
      |> List.map (\(x, y) -> (shape.position.x + x, shape.position.y + y ))
      |> coordTuplesToLayout
  else
    shape.piece.layout
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

type alias Color = String

type alias Piece =
  { layout: List ( Int, Int )
  , rotates: Bool
  , color: Color
  }

square : Piece
square =
  { layout =
    [ ( 0, 0 )
    , ( 1, 0 )
    , ( 0, 1 )
    , ( 1, 1 )
    ]
  , rotates = False
  , color = "Red"
  }

line : Piece
line =
  { layout =
    [ ( -1, 0 )
    , ( 0, 0 )
    , ( 1, 0 )
    , ( 2, 0 )
    ]
  , rotates = True
  , color = "Blue"
  }

l : Piece
l =
  { layout =
    [ ( 0, -1 )
    , ( 0, 0 )
    , ( 0, 1 )
    , ( 1, 1 )
    ]
  , rotates = True
  , color = "Yellow"
  }

setRotation : Rotation -> Model -> Model
setRotation rotation model =
  { model
    | shape =
    { position = model.shape.position
    , piece = model.shape.piece
    , rotation = rotation
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

updateShapeX : Int -> Model -> Model
updateShapeX change model =
  { model | shape =
    { position = updatePositionX change model.shape.position
    , piece = model.shape.piece
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
    , piece = model.shape.piece
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
    , piece = model.shape.piece
    , rotation = model.shape.rotation
    , timeSinceMove = timeSinceMove
    }
  }

collidesBelow : PlacedPieces -> Position -> Bool
collidesBelow placedPieces position =
  let
    belowPosition =
      { position | y = position.y + 1 }
  in
    if belowPosition.y >= playArea.height then
      True
    else if collidesPlaced belowPosition placedPieces then
      True
    else
      False

collidesLeft : PlacedPieces -> Position -> Bool
collidesLeft placedPieces position =
  let
    leftPosition =
      { position | x = position.x - 1 }
  in
    if leftPosition.x < 0 then
      True
    else if collidesPlaced leftPosition placedPieces then
      True
    else
      False

collidesRight : PlacedPieces -> Position -> Bool
collidesRight placedPieces position =
  let
    rightPosition = { position | x = position.x + 1 }
  in
    if rightPosition.x >= playArea.width then
      True
    else if collidesPlaced rightPosition placedPieces then
      True
    else
      False

getPlacedVal : Int -> Int -> PlacedPieces -> Bool
getPlacedVal x y placedPieces =
  placedPieces
    |> Array.get x
    |> Maybe.withDefault (False |> Array.repeat playArea.height)
    |> Array.get y
    |> Maybe.withDefault False

collidesPlaced : Position -> PlacedPieces -> Bool
collidesPlaced position placedPieces =
  placedPieces |> getPlacedVal position.x position.y

newShape : Model -> Model
newShape model =
  { model | shape =
    { position = startPosition
    , piece = model.shape.piece
    , rotation = model.shape.rotation
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

addToPlacedPieces : Model -> Model
addToPlacedPieces model =
  { model
    | placedPieces = placePiece model.shape model.placedPieces
  }

placePiece : Shape -> PlacedPieces -> PlacedPieces
placePiece shape placedPieces =
  List.foldl (setPlaced True) placedPieces (mapPiece shape)

setPlaced : Bool -> Position -> PlacedPieces -> PlacedPieces
setPlaced isSet position placedPieces =
  let
    column = Array.get position.x placedPieces
  in
    case column of
      Nothing ->
        placedPieces
      Just column ->
        Array.set position.x (Array.set position.y isSet column) placedPieces

clearFullRows : Model -> Model
clearFullRows model =
  let
    placedPieces = model.placedPieces
    completedRows = getCompletedRows placedPieces
  in
    { model
      | placedPieces = Array.map (\col -> clearIfRowComplete completedRows col) placedPieces
    }

getCompletedRows : PlacedPieces -> Array Bool
getCompletedRows placedPieces =
  let
    fullCol = Array.repeat playArea.height True
  in
    Array.foldl (\col prevCol -> bothTrue col prevCol ) fullCol placedPieces

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
        updateShapeX 1 rotated
      else if okToMoveLeft rotated then
        updateShapeX -1 rotated
      else
        model
    else
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

collides : PlacedPieces -> Position -> Bool
collides placedPieces position =
  if collidesPlaced position placedPieces || outOfBounds position then
    True
  else
    False

shapeCollides : Model -> Bool
shapeCollides model =
  model.shape
    |> mapPiece
    |> List.any (collides model.placedPieces)

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