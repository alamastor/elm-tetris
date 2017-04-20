module Model exposing
  ( Model
  , Rotation(..)
  , PlacedPieces
  , Position
  , ActivePiece
  , Piece
  , square
  , line
  , l
  , triangle
  , mirrorL
  , Color
  , playArea
  , startPosition
  , pixelsPerUnit
  , toSvgPix
  , mapPiece
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
  , startSpeed
  )

import Time exposing (Time)
import Array exposing (Array)
import Array.Extra


type alias Model =
  { activePiece : ActivePiece
  , placedPieces: PlacedPieces
  , game: Game
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

startSpeed : UnitsPerSecond
startSpeed = 5

type alias ActivePiece =
  { position: Position
  , piece: Piece
  , rotation: Rotation
  }

type alias Game =
  { speed: UnitsPerSecond
  , timeSinceMove: Time
  , paused: Bool
  }

type alias PlacedPieces =
  Array (Array (Maybe Color))

type Rotation = Zero | Ninty | OneEighty | TwoSeventy

type alias Layout = List Position

type alias LayoutSquare = ( LayoutOffset, LayoutOffset )
type alias LayoutOffset = Int

mapPiece : ActivePiece -> Layout
mapPiece activePiece =
  if activePiece.piece.rotates then
    activePiece.piece.layout
      |> rotateLayout activePiece.rotation
      |> List.map (\(x, y) -> (activePiece.position.x + x, activePiece.position.y + y ))
      |> coordTuplesToLayout
  else
    activePiece.piece.layout
      |> List.map (\(x, y) -> (activePiece.position.x + x, activePiece.position.y + y ))
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
  , color = "#779ecb"
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
  , color = "#966df6"
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
  , color = "#03c03c"
  }

triangle : Piece
triangle =
  { layout =
    [ ( -1, 0 )
    , ( 0, 0 )
    , ( 1, 0 )
    , ( 0, 1 )
    ]
  , rotates = True
  , color = "#ff6961"
  }

mirrorL : Piece
mirrorL =
  { layout =
    [ ( 0, 1 )
    , ( 0, 0 )
    , ( 0, -1 )
    , ( -1, 1 )
    ]
  , rotates = True
  , color = "#ffb347"
  }

setRotation : Rotation -> Model -> Model
setRotation rotation model =
  { model
    | activePiece =
    { position = model.activePiece.position
    , piece = model.activePiece.piece
    , rotation = rotation
    }
  }

updateShapeX : Int -> Model -> Model
updateShapeX change model =
  { model | activePiece =
    { position = updatePositionX change model.activePiece.position
    , piece = model.activePiece.piece
    , rotation = model.activePiece.rotation
    }
  }

updatePositionX : Int -> Position -> Position
updatePositionX change position =
  { position | x = position.x + change}

updateShapeY : Int -> Model -> Model
updateShapeY change model =
  { model | activePiece =
    { position = updatePositionY change model.activePiece.position
    , piece = model.activePiece.piece
    , rotation = model.activePiece.rotation
    }
  }

updatePositionY : Int -> Position -> Position
updatePositionY change position =
  { position | y = position.y + change}

updateTimeSinceMove : Time -> Model -> Model
updateTimeSinceMove timeSinceMove model =
  let game = model.game
  in
  { model
    | game = { game
      | timeSinceMove = timeSinceMove
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

getPlacedVal : Int -> Int -> PlacedPieces -> Maybe Color
getPlacedVal x y placedPieces =
  placedPieces
    |> Array.get x
    |> Maybe.withDefault (Nothing |> Array.repeat playArea.height)
    |> Array.get y
    |> Maybe.withDefault Nothing

collidesPlaced : Position -> PlacedPieces -> Bool
collidesPlaced position placedPieces =
  let color = placedPieces |> getPlacedVal position.x position.y
  in
    case color of
      Just color ->
        True
      Nothing ->
        False

newShape : Model -> Model
newShape model =
  { model | activePiece =
    { position = startPosition
    , piece = model.activePiece.piece
    , rotation = model.activePiece.rotation
    }
  }

addToPlacedPieces : Model -> Model
addToPlacedPieces model =
  { model
    | placedPieces = placePiece model.activePiece model.placedPieces
  }

placePiece : ActivePiece -> PlacedPieces -> PlacedPieces
placePiece activePiece placedPieces =
  List.foldl (setPlaced activePiece.piece.color) placedPieces (mapPiece activePiece)

setPlaced : Color -> Position -> PlacedPieces -> PlacedPieces
setPlaced color position placedPieces =
  let
    column = Array.get position.x placedPieces
  in
    case column of
      Nothing ->
        placedPieces
      Just column ->
        Array.set position.x (Array.set position.y (Just color) column) placedPieces

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
    Array.foldl ( \col prevCol -> bothColored col prevCol ) fullCol placedPieces

clearIfRowComplete : Array Bool -> Array (Maybe Color) -> Array (Maybe Color)
clearIfRowComplete completedRows col =
  col
    |> Array.indexedMap (\y color -> (y, color))
    |> Array.Extra.removeWhen (\(y, _) -> rowIsComplete y completedRows)
    |> Array.map (\(_, color) -> color)
    |> Array.Extra.resizerRepeat playArea.height Nothing

rowIsComplete : Int -> Array Bool -> Bool
rowIsComplete y completedRows =
  completedRows
    |> Array.get y
    |> Maybe.withDefault False

bothColored : Array (Maybe Color) -> Array Bool -> Array Bool
bothColored array1 array2 =
  Array.Extra.map2 ( \color isSet -> isSet && (isColored color) ) array1 array2

isColored : Maybe Color -> Bool
isColored color =
  case color of
    Just color ->
      True
    Nothing ->
      False

tryRotate : Rotation -> Model -> Model
tryRotate rotation model =
  let
    rotated = setRotation rotation model
  in
    if activePieceCollides rotated then
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

activePieceCollides : Model -> Bool
activePieceCollides model =
  model.activePiece
    |> mapPiece
    |> List.any (collides model.placedPieces)

okToMoveRight : Model -> Bool
okToMoveRight model =
  model
    |> updateShapeX 1
    |> activePieceCollides
    |> not

okToMoveLeft : Model -> Bool
okToMoveLeft model =
  model
    |> updateShapeX -1
    |> activePieceCollides
    |> not