module View exposing(view)

import Html exposing (program, Html)
import Svg exposing (Svg, svg, rect)
import Svg.Attributes exposing (x, y, width, height, stroke, fill)
import Array exposing (Array)

import Model exposing
  ( Model
  , Position
  , PlacedPieces
  , ActivePiece
  , Color
  , playArea
  , toSvgPix
  , mapPiece
  )

import Messages exposing (Msg)


shapeRects : ActivePiece -> List (Svg Msg)
shapeRects shape =
  shape
    |> mapPiece
    |> List.map (layoutRect shape.piece.color)

layoutRect : Color -> Position -> Svg Msg
layoutRect color position =
  rect
    [ toSvgPix position.x |> Svg.Attributes.x
    , toSvgPix position.y |> Svg.Attributes.y
    , toSvgPix 1 |> width
    , toSvgPix 1 |> height
    , fill color
    ] []

placedActivePiecesRects : PlacedPieces -> List(Svg Msg)
placedActivePiecesRects placedActivePieces =
  placedActivePieces
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
      [ [ rect [ toSvgPix playArea.width |> width, toSvgPix playArea.height |> height, fill "none" ] [] ]
      , shapeRects model.activePiece
      , ( placedActivePiecesRects model.placedPieces )
      ]
    )