port module Auth
    exposing
        ( User
        , signIn
        , signOut
        , authStateChanged
        )


port signIn : () -> Cmd msg


type alias User =
    { name : String
    }


port authStateChanged : (Maybe User -> msg) -> Sub msg


port signOut : () -> Cmd msg
