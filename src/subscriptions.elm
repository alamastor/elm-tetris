module Subscriptions exposing (subscriptions)

import AnimationFrame
import Keyboard
import Model exposing (Model)
import Messages exposing (Msg(..))
import Auth


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ AnimationFrame.diffs FrameMsg
        , Keyboard.downs KeyMsg
        , Auth.authStateChanged AuthStateChanged
        ]
