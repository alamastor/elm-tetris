module Messages exposing (Msg(..))

import Time exposing (Time)
import Keyboard
import Model
import Auth exposing (User)


type Msg
    = NoOp
    | FrameMsg Time
    | KeyMsg Keyboard.KeyCode
    | UpdateBothPieces ( Model.Piece, Model.Piece )
    | UpdateNextPiece Model.Piece
    | SignIn
    | SignOut
    | AuthStateChanged (Maybe User)
