module Api exposing (sendScore)

import Http
import Messages exposing (Msg(..))
import Json.Encode exposing (object, int)
import Auth exposing (User)


sendScore : Int -> User -> Cmd Msg
sendScore score user =
    let
        url =
            "https://elm-tetris.firebaseio.com/score.json?auth=" ++ user.token

        request =
            Http.request
                { method = "PUT"
                , headers = []
                , url = url
                , body = Http.jsonBody (object [ ( "score", int score ) ])
                , expect = Http.expectStringResponse (\_ -> Ok ())
                , timeout = Nothing
                , withCredentials = False
                }
    in
        Http.send SentScore request
