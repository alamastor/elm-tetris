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
  , Unit
  , Piece
  , pixelsPerUnit
  , Color
  , playArea
  , mapPiece
  )
import Messages exposing (Msg)


toSvgUnits : Unit -> String
toSvgUnits units =
  units * pixelsPerUnit |> toString

toPx : Unit -> String
toPx units =
  toString (units * pixelsPerUnit) ++ "px"

activePieceRects : ActivePiece -> List (Svg Msg)
activePieceRects activePiece =
  activePiece
    |> mapPiece
    |> List.map (layoutRect activePiece.piece.color)

nextPieceRects : Piece -> List (Svg Msg)
nextPieceRects piece =
  piece.layout
    |> List.map (layoutPieceRect piece.color (pieceMins piece))

layoutRect : Color -> Position -> Svg Msg
layoutRect color position =
  rect
    [ toSvgUnits position.x |> Svg.Attributes.x
    , toSvgUnits position.y |> Svg.Attributes.y
    , toSvgUnits 1 |> width
    , toSvgUnits 1 |> height
    , fill color
    ] []

layoutPieceRect : Color -> (Int, Int) -> (Int, Int) -> Svg Msg
layoutPieceRect color (minX, minY) (x, y) =
  rect
    [ toSvgUnits (x - minX) |> Svg.Attributes.x
    , toSvgUnits (y - minY) |> Svg.Attributes.y
    , toSvgUnits 1 |> width
    , toSvgUnits 1 |> height
    , fill color
    ] []

pieceMins : Piece -> (Unit, Unit)
pieceMins piece =
  piece.layout
    |> List.foldl (\(x, y) (minX, minY)  -> (min x minX, min y minY)) (99999, 9999)


placedActivePiecesRects : PlacedPieces -> List(Svg Msg)
placedActivePiecesRects placedPieces =
  placedPieces
    |> Array.indexedMap getIndexPair
    |> Array.toList
    |> List.concat
    |> List.map (\(xIdx, yIdx, color) -> (rect
      [ toSvgUnits xIdx |> x
      , toSvgUnits yIdx |> y
      , toSvgUnits 1 |> width
      , toSvgUnits 1 |> height
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
  div [ style [ ("width", "100%"), ("display", "flex") ]]
  [ div [style [ ("width", "calc((100% - " ++ toPx playArea.width ++ ") / 2)") ] ] []
  , div [style [ ("width", (toSvgUnits playArea.width) ++ "px" ) ]]
    [ svg [ toSvgUnits playArea.width |> width, toSvgUnits playArea.height |> height ]
        ( List.concat
          [ [ rect [ toSvgUnits playArea.width |> width, toSvgUnits playArea.height |> height, fill "bisque" ] [] ]
          , activePieceRects model.activePiece
          , ( placedActivePiecesRects model.placedPieces )
          ]
        )
    ]
  , div [style [ ("width", "calc((100% - " ++ toPx playArea.width ++ ") / 2)") ] ]
    [ svg [ toSvgUnits 4 |> width ] (nextPieceRects model.nextPiece) ]
  ]