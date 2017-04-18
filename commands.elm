module Commands exposing (randomShape)

import Random
import Random.Extra

import Messages exposing (Msg(UpdateLayout))
import Model exposing (ShapeName(Square, Line, L))

randomShape : Cmd Msg
randomShape =
  Random.Extra.sample [Square, Line, L]
    |> Random.map (Maybe.withDefault Square)
    |> Random.generate UpdateLayout