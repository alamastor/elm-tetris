module Commands exposing (randomShape)

import Random
import Random.Extra

import Messages exposing (Msg(UpdatePiece))
import Model exposing (square, line, l, triangle, mirrorL)

randomShape : Cmd Msg
randomShape =
  Random.Extra.sample [square, line, l, triangle, mirrorL]
    |> Random.map (Maybe.withDefault square)
    |> Random.generate UpdatePiece