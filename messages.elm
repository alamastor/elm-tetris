module Messages exposing (Msg(..))

import Time exposing (Time)
import Keyboard

import Model

type Msg
  = NoOp
  | FrameMsg Time
  | KeyMsg Keyboard.KeyCode
  | UpdateLayout Model.ShapeName