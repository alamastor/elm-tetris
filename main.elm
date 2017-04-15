import Html exposing (program, Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (x, y, width, height, stroke, fill)
import AnimationFrame
import Time exposing (Time)


-- MAIN

main : Program Never Model Msg
main =
  program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- BOARD

pixelsPerUnit : Int
pixelsPerUnit = 20

type alias Unit = Int

toSvgPix : Unit -> String
toSvgPix units =
  units * pixelsPerUnit |> toString

type alias Rectangle =
  { width: Unit
  , height: Unit
  }

playArea : Rectangle
playArea =
  { width = 21
  , height = 30
  }

type alias Position =
  { x: Unit
  , y: Unit
  }

type alias UnitsPerSecond = Float

speed : UnitsPerSecond
speed = 2



-- MODEL

type alias Model =
  { shape :
    { position: Position
    , timeSinceMove: Time
    }
  }

init : ( Model, Cmd Msg )
init =
  ( { shape =
      { position =
        { x = 10
        , y = 0
        }
      , timeSinceMove = 0
      }
    }
  , Cmd.none
  )



-- MESSAGES

type Msg
  = NoOp
  | FrameMsg Time



-- UPDATE

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    FrameMsg diff ->
      let timeSinceMove = model.shape.timeSinceMove + diff
      in
        if Time.inSeconds timeSinceMove >= 1 / speed then
          ( model
            |> updateTimeSinceMove (Time.inSeconds timeSinceMove - 1 / speed)
            |> updateShapeY 1
          , Cmd.none
          )
        else
          ( model |> updateTimeSinceMove timeSinceMove, Cmd.none )

updateShapeY : Int -> Model -> Model
updateShapeY change model =
  { model | shape =
    { position = updatePositionY change model.shape.position
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

updatePositionY : Int -> Position -> Position
updatePositionY change position =
  { position | y = position.y + change}

updateTimeSinceMove : Time -> Model -> Model
updateTimeSinceMove timeSinceMove model =
  { model | shape =
    { position = model.shape.position
    , timeSinceMove = timeSinceMove
    }
  }



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs FrameMsg
    ]



-- VIEW

view : Model -> Html Msg
view model =
  svg [ toSvgPix playArea.width |> width, toSvgPix playArea.height |> height ]
    [ rect [ toSvgPix playArea.width |> width, toSvgPix playArea.height |> height, stroke "black", fill "none" ] []
    , rect
      [ toSvgPix model.shape.position.x |> x
      , toSvgPix model.shape.position.y |> y
      , toSvgPix 1 |> width
      , toSvgPix 1 |> height
      ] []
    ]