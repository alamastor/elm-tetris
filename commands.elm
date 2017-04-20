module Commands exposing (randomShape)

import Random
import Random.Extra

import Messages exposing (Msg(UpdatePiece))
import Model exposing (square, line, l)

randomShape : Cmd Msg
randomShape =
  Random.Extra.sample [square, line, l]
    |> Random.map (Maybe.withDefault square)
    |> Random.generate UpdatePiece