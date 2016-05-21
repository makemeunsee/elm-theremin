port module Theremin exposing (..)


import Html exposing (..)
import Html.Attributes exposing (style)
import Html.App as Html
import Mouse exposing (Position)
import Task
import Color
import Window exposing (Size)
import Debug


main =
  Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL


type alias Model =
    { position : Position
    , size : Size
    }


init : ( Model, Cmd Msg )
init =
  ( Model (Position 0 0) (Size 0 0), initialSizeCmd )


type alias NormedMousePosition =
    { x : Float
    , y : Float
    }


normMouse : Model -> NormedMousePosition
normMouse model =
  let
    w = toFloat model.size.width
    h = toFloat model.size.height
    x = toFloat model.position.x
    y = toFloat model.position.y
    xNormed = x / w -- TODO logarithmic scaling??
    yNormed = (h - y) / h
  in
    NormedMousePosition xNormed yNormed


-- UPDATE


type Msg
    = MouseMove Position
    | ScreenResize Size
    --| Suggest (List String)


initialSizeCmd : Cmd Msg
initialSizeCmd =
  Task.perform (\_ -> ScreenResize (Size 1 1)) ScreenResize Window.size


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let
    newModel = updateHelp msg model
  in
    ( newModel, audio (normMouse newModel) )


updateHelp msg model =
  case msg of
    MouseMove position -> { model | position = position }
    ScreenResize size -> { model | size = size }


-- PORTS


--port suggestions : (List String -> msg) -> Sub msg


port audio : NormedMousePosition -> Cmd msg


-- SUBSCRIPTIONS


mouseSubscription = Mouse.moves MouseMove


screenSubscription = Window.resizes ScreenResize
  

subscriptions: Model -> Sub Msg
subscriptions _ =
  Sub.batch [ mouseSubscription
            , screenSubscription
            --, suggestions Suggest
            ]


-- VIEW


(=>) = (,)


view : Model -> Html Msg
view model =
  let
    (x, y) = normedPosition model
  in
    div [ style
            [ "backgroundColor" => rgb x y
            , "width" => px model.size.width
            , "height" => px model.size.height
            , "left" => "0"
            , "top" => "0"
            , "position" => "absolute"
            ]
        ]
        [  ]


normedPosition : Model -> (Float, Float)
normedPosition { position, size } =
  let
    side = min size.width size.height
      |> toFloat
      |> (*) 0.5
  in
    ((toFloat (position.x - size.width // 2)) / side, (toFloat (size.height // 2 - position.y)) / side)


px : Int -> String
px number =
  toString number ++ "px"


rgb : Float -> Float -> String
rgb x y =
  let
    theta = atan2 y x
    d = sqrt (x*x + y*y)
    thetaR = abs theta
    thetaG = (abs << withinPi) (theta - 2 * pi / 3)
    thetaB = (abs << withinPi) (theta + 2 * pi / 3)
    cmp = toString << floor << toComponent d
  in  
    "rgb(" ++ cmp thetaR ++ "," ++ cmp thetaG ++ "," ++ cmp thetaB ++ ")" 


withinPi : Float -> Float
withinPi value =
  if value > pi then
    value - 2 * pi
  else if value < -pi then
    value + 2 * pi
  else
    value


toComponent : Float -> Float -> Float
toComponent d theta =
  let
    k = 255 * min 1 d
  in
    if theta < pi / 3 then
      k
    else if theta < 2 * pi / 3 then
      k * (2 * pi / 3 - theta) / pi * 3
    else
      0