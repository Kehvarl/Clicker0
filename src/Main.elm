module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (on, onClick, onMouseDown, onMouseUp)
import Json.Decode
import Random
import Svg
import Svg.Attributes as SA
import Task
import Time



--MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Circle =
    { id : Int
    , x : Int
    , y : Int
    , size : Int
    , color : Color
    , mouseDown : Bool
    }


type Color
    = Blue
    | Red
    | Green
    | Black


type alias Model =
    { circles : List Circle
    , next : Int
    , nextX : Int
    , nextY : Int
    }



--INIT


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model [ Circle 0 100 100 5 Blue False ] 1 75 75, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick



--UPDATE


type Msg
    = Click Int
    | Down Int
    | Up Int
    | PosX Int
    | PosY Int
    | Tick Time.Posix


randomPos : Random.Generator Int
randomPos =
    Random.int 0 100


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click id ->
            ( model, Cmd.none )

        Down id ->
            let
                checkCircle c =
                    if c.id == id then
                        { c | mouseDown = True }

                    else
                        c
            in
            ( { model | circles = List.map checkCircle model.circles }
            , Cmd.none
            )

        Up id ->
            let
                checkCircle c =
                    if c.id == id then
                        { c | mouseDown = False }

                    else
                        c
            in
            ( { model | circles = List.map checkCircle model.circles }
            , Cmd.none
            )

        PosX x ->
            ( { model | nextX = x }
            , Random.generate PosY randomPos
            )

        PosY y ->
            ( { model | nextY = y }
            , Cmd.none
            )

        Tick _ ->
            let
                checkCircle c =
                    if c.mouseDown then
                        if c.size > 20 then
                            popCircle model.nextX model.nextY c

                        else
                            { c | size = c.size + 1 }

                    else
                        c
            in
            ( { model | circles = List.map checkCircle model.circles }
            , Random.generate PosX randomPos
            )


popCircle : Int -> Int -> Circle -> Circle
popCircle x y circle =
    { circle
        | size = 5
        , color = nextColor circle.color
        , x = x
        , y = y
        , mouseDown = False
    }


nextColor : Color -> Color
nextColor color =
    case color of
        Blue ->
            Red

        Red ->
            Green

        Green ->
            Black

        Black ->
            Blue



--VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ Svg.svg
            [ SA.viewBox "0,0,500,500" ]
            (List.map viewCircle model.circles)
        ]


viewCircle : Circle -> Html Msg
viewCircle c =
    Svg.circle
        [ SA.fill "white"
        , SA.stroke (stringFromColor c.color)
        , SA.cx (String.fromInt c.x)
        , SA.cy (String.fromInt c.y)
        , SA.r (String.fromInt c.size)

        --, onClick (Click c.id)
        , onMouseDown (Down c.id)
        , onMouseUp (Up c.id)
        , on "touchstart" (Json.Decode.succeed (Down c.id))
        , on "touchend" (Json.Decode.succeed (Up c.id))
        ]
        []


stringFromColor : Color -> String
stringFromColor color =
    case color of
        Blue ->
            "blue"

        Red ->
            "red"

        Green ->
            "green"

        Black ->
            "black"
