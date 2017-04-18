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
  )

import Time exposing (Time)
import Array exposing (Array)


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
speed = 7

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