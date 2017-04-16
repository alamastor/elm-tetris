import Html exposing (program, Html)
import Svg exposing (svg, rect)
import Svg.Attributes exposing (x, y, width, height, stroke, fill)
import AnimationFrame
import Time exposing (Time)
import Keyboard


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
speed = 4



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
  | KeyMsg Keyboard.KeyCode



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
            |> moveDown
          , Cmd.none
          )
        else
          ( model |> updateTimeSinceMove timeSinceMove, Cmd.none )
    KeyMsg keyCode ->
      if keyCode == 37 then
        ( moveLeft model, Cmd.none )
      else if keyCode == 39 then
        ( model |> moveRight, Cmd.none )
      else if keyCode == 40 then
        ( model |> moveDown, Cmd.none )
      else
        ( model, Cmd.none )

moveLeft : Model -> Model
moveLeft model =
  if model |> collidesHoriz -1 then
    model
  else
    updateShapeX -1 model

moveRight : Model -> Model
moveRight model =
  if model |> collidesHoriz 1 then
    model
  else
    updateShapeX 1 model

moveDown : Model -> Model
moveDown model =
  if model |> collidesBottom 1 then
    model
  else
    updateShapeY 1 model

updateShapeX : Int -> Model -> Model
updateShapeX change model =
  { model | shape =
    { position = updatePositionX change model.shape.position
    , timeSinceMove = model.shape.timeSinceMove
    }
  }

updatePositionX : Int -> Position -> Position
updatePositionX change position =
  { position | x = position.x + change}

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

collidesBottom : Int -> Model -> Bool
collidesBottom change model =
  if model.shape.position.y + change >= playArea.height then
    True
  else
    False

collidesHoriz : Int -> Model -> Bool
collidesHoriz change model =
  if model.shape.position.x + change < 0 then
    True
  else if model.shape.position.x + change >= playArea.width then
    True
  else
    False



-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ AnimationFrame.diffs FrameMsg
    , Keyboard.downs KeyMsg
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