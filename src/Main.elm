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
    , newCircle : Int
    }



--INIT


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model [ Circle 0 75 300 5 Blue False ] 1 75 75 100, Cmd.none )


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
    Random.uniform 55 [ 75 ]


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
            ( { model | nextY = 300 }
            , Cmd.none
            )

        Tick _ ->
            let
                checkCircle c =
                    if c.mouseDown then
                        if c.size > 20 then
                            popCircle model.nextX model.nextY c

                        else
                            { c | size = c.size + 1, y = c.y - 1 }

                    else
                        { c | y = c.y - 1 }
            in
            ( { model
                | circles = List.map checkCircle model.circles
                , newCircle = model.newCircle - 1
              }
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
            [ SA.height "300", SA.width "400" ]
            (List.map viewCircle model.circles)
        , div [] [ viewFactory ]
        ]


viewFactory : Html Msg
viewFactory =
    Svg.svg
        []
        [ Svg.rect
            [ SA.stroke "black"
            , SA.fill "darkgrey"
            , SA.x "50"
            , SA.y "0"
            , SA.width "10"
            , SA.height "15"
            ]
            []
        , Svg.rect
            [ SA.stroke "black"
            , SA.fill "darkgrey"
            , SA.x "70"
            , SA.y "0"
            , SA.width "10"
            , SA.height "15"
            ]
            []
        , Svg.polygon
            [ SA.stroke "black"
            , SA.fill "grey"
            , SA.points "90,15 110, 5 110, 15 130,5 130,15 150,5 150,15"
            ]
            []
        , Svg.rect
            [ SA.stroke "black"
            , SA.fill "darkred"
            , SA.x "50"
            , SA.y "15"
            , SA.width "100"
            , SA.height "25"
            ]
            []
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
