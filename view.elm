module View exposing(view)

import Html exposing (program, Html, div, text)
import Html.Attributes exposing (style)
import Svg exposing (Svg, svg, rect, text_)
import Svg.Attributes exposing (x, y, width, height, stroke, fill, textAnchor, fontFamily, fontSize)
import Array exposing (Array)

import Model exposing
  ( Model
  , Position
  , PlacedPieces
  , ActivePiece
  , Unit
  , Piece
  , GameState (Paused, Active, GameOver)
  , pixelsPerUnit
  , Color
  , playArea
  , mapPiece
  , pieceMins
  , pieceSize
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

statusText : Model -> String
statusText model =
  case model.game.gameState of
    Paused ->
      "Paused"
    GameOver ->
      "Game Over"
    _ ->
      ""

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
          , [ text_
              [ toSvgUnits playArea.width |> width
              , toString (toFloat (pixelsPerUnit * playArea.width) / 2) |> x
              , y "100"
              , textAnchor "middle"
              , fontFamily "sans-serif"
              , fontSize "2em"
              ] [ statusText model |> Svg.text  ] ]
          ]
        )
    ]
  , div [ style [ ("width", "calc((100% - " ++ toPx playArea.width ++ ") / 2)") ] ]
    [ div [ style
      [ ("font-family", "sans-serif")
      , ("margin", "1rem 0")
      , ("text-align", "center")
      ]
    ] [ text "Next Piece" ]
    , div [ style [ ("display", "flex"), ("justify-content", "center") ] ]
      [ svg
        [ (pieceSize model.nextPiece).width |> toSvgUnits |> width
        , 3 |> toSvgUnits |> height
        ] (nextPieceRects model.nextPiece)
      ]
    , div [ style
      [ ("font-family", "sans-serif")
      , ("margin", "1rem 0")
      , ("text-align", "center")
      ]
    ] [ text ("Score: " ++ (toString model.game.score)) ]
    ]
  ]