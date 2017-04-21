module Commands exposing (randomNextPiece, randomPiecePair)

import Random
import Random.Extra

import Messages exposing (Msg(UpdateBothPieces, UpdateNextPiece))
import Model exposing (square, line, l, triangle, mirrorL)

randomNextPiece : Cmd Msg
randomNextPiece =
  Random.Extra.sample [square, line, l, triangle, mirrorL]
    |> Random.map (Maybe.withDefault square)
    |> Random.generate UpdateNextPiece

randomPiecePair : Cmd Msg
randomPiecePair =
  let randomPiece = Random.Extra.sample [square, line, l, triangle, mirrorL]
    |> Random.map (Maybe.withDefault square)
  in
    Random.pair randomPiece randomPiece
      |> Random.generate UpdateBothPieces