module View exposing(view)

import Html exposing (program, Html, div)
import Html.Attributes exposing (style)
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
placedActivePiecesRects placedPieces =
  placedPieces
    |> Array.indexedMap getIndexPair
    |> Array.toList
    |> List.concat
    |> List.map (\(xIdx, yIdx, color) -> (rect
      [ toSvgPix xIdx |> x
      , toSvgPix yIdx |> y
      , toSvgPix 1 |> width
      , toSvgPix 1 |> height
      , colorOrNone color |> fill
      ] []))

colorOrNone : Maybe Color -> String
colorOrNone color =
  case color of
    Just color ->
      color
    Nothing ->
      "none"

getIndexPair : Int -> Array (Maybe Color) -> List (Int, Int, Maybe Color)
getIndexPair xIdx array =
  array
    |> Array.indexedMap (\yIdx color -> (xIdx, yIdx, color))
    |> Array.toList

view : Model -> Html Msg
view model =
  div [ style [ ("width", "100%") ]]
  [ div [style [ ("margin", "0 auto"), ("width", (toSvgPix playArea.width) ++ "px" ) ]]
    [ svg [ toSvgPix playArea.width |> width, toSvgPix playArea.height |> height ]
        ( List.concat
          [ [ rect [ toSvgPix playArea.width |> width, toSvgPix playArea.height |> height, fill "bisque" ] [] ]
          , shapeRects model.activePiece
          , ( placedActivePiecesRects model.placedPieces )
          ]
        )
    ]
  ]