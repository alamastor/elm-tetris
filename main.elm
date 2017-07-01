module Main exposing (..)

import Html exposing (program)
import View exposing (view)
import Subscriptions exposing (subscriptions)
import Model exposing (Model)
import Messages exposing (Msg)
import Update exposing (update, init)


main : Program Never Model Msg
main =
    program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
