module Main exposing (..)

import Browser
import Color
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
    , size : Float
    , vX : Int
    , vY : Int
    , vR : Float
    , color : Color.Color
    , mouseDown : Bool
    , popped : Bool
    }


type alias Model =
    { circles : List Circle
    , next : Int
    , nextX : Int
    , nextY : Int
    , nextCircle : Int
    }



--INIT


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model [ Circle 0 75 300 5 0 1 0 Color.Blue False False ] 1 75 300 25, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick



--UPDATE


type Msg
    = Down Int
    | Up Int
    | PosX Int
    | Tick Time.Posix


randomPos : Random.Generator Int
randomPos =
    Random.uniform 55 [ 75 ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Down id ->
            let
                checkCircle c =
                    if c.id == id then
                        { c | mouseDown = True }

                    else
                        c
            in
            ( { model | circles = List.map checkCircle model.circles }
            , Random.generate PosX randomPos
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
            , Cmd.none
            )

        Tick _ ->
            ( canSpawnCircle
                (updateCircles model)
            , Random.generate PosX randomPos
            )


updateCircles : Model -> Model
updateCircles model =
    cleanupCircles
        (spawnCircles
            { model
                | circles =
                    List.map pressCircle
                        (List.map updateCircle model.circles)
            }
        )


cleanupCircles : Model -> Model
cleanupCircles model =
    { model | circles = List.filter (\c -> c.y > 0) model.circles }


pressCircle : Circle -> Circle
pressCircle circle =
    if circle.mouseDown then
        { circle | vR = circle.vR + 0.05 }

    else
        circle


updateCircle : Circle -> Circle
updateCircle circle =
    popCircle
        { circle
            | y = circle.y - circle.vY
            , size = circle.size + circle.vR
        }


popCircle : Circle -> Circle
popCircle circle =
    if circle.size > 20 then
        { circle | popped = True }

    else
        circle


canSpawnCircle : Model -> Model
canSpawnCircle model =
    if model.nextCircle == 0 then
        { model
            | circles =
                model.circles
                    ++ [ spawnCircle model Color.Blue ]
            , next = model.next + 1
            , nextCircle = 25
        }

    else
        { model | nextCircle = model.nextCircle - 1 }


spawnCircles : Model -> Model
spawnCircles model =
    model


spawnCircle : Model -> Color.Color -> Circle
spawnCircle model color =
    Circle
        model.next
        model.nextX
        model.nextY
        5
        0
        1
        0
        color
        False
        False



--VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ Svg.svg
            [ SA.height "300", SA.width "400" ]
            (List.map viewCircle (List.filter (\c -> c.popped == False) model.circles))
        , div [] [ viewFactory ]
        , div [] []
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
        , SA.fillOpacity "0"
        , SA.stroke (Color.stringFromColor c.color)
        , SA.cx (String.fromInt c.x)
        , SA.cy (String.fromInt c.y)
        , SA.r (String.fromFloat c.size)

        --, onClick (Click c.id)
        , onMouseDown (Down c.id)
        , onMouseUp (Up c.id)
        , on "touchstart" (Json.Decode.succeed (Down c.id))
        , on "touchend" (Json.Decode.succeed (Up c.id))
        ]
        []
