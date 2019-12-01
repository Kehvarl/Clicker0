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
    , size : Int
    , vX : Int
    , vY : Int
    , vR : Int
    , color : Color.Color
    , mouseDown : Bool
    }


type alias Model =
    { circles : List Circle
    , next : Int
    , nextX : Int
    , nextY : Int
    , nextCircle : Int
    , automate : List Color.Color
    }



--INIT


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model [ Circle 0 75 300 5 0 1 0 Color.Blue False ] 1 75 75 25 [ Color.Red ], Cmd.none )


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
            ( spawnCircle
                { model
                    | circles =
                        List.map
                            (raiseCircle model.nextX model.nextY)
                            model.circles
                }
            , Random.generate PosX randomPos
            )


raiseCircle : Int -> Int -> Circle -> Circle
raiseCircle nextX nextY circle =
    if circle.mouseDown then
        if circle.size > 20 then
            popCircle nextX nextY circle

        else
            { circle | size = circle.size + 1, y = circle.y - 1 }

    else
        { circle | y = circle.y - circle.vY }


popCircle : Int -> Int -> Circle -> Circle
popCircle x y circle =
    { circle
        | size = 5
        , color = Color.nextColor circle.color
        , x = x
        , y = y
        , mouseDown = False
    }


spawnCircle : Model -> Model
spawnCircle model =
    if model.nextCircle == 0 then
        { model
            | circles =
                cleanupCircles
                    (model.circles
                        ++ [ Circle
                                model.next
                                model.nextX
                                model.nextY
                                5
                                0
                                1
                                0
                                Color.Blue
                                False
                           ]
                    )
            , next = model.next + 1
            , nextCircle = 25
        }

    else
        { model | nextCircle = model.nextCircle - 1 }


cleanupCircles : List Circle -> List Circle
cleanupCircles circleList =
    List.filter (\c -> c.y > 0) circleList



--VIEW


view : Model -> Html Msg
view model =
    div
        []
        [ Svg.svg
            [ SA.height "300", SA.width "400" ]
            (List.map viewCircle model.circles)
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
        , SA.r (String.fromInt c.size)

        --, onClick (Click c.id)
        , onMouseDown (Down c.id)
        , onMouseUp (Up c.id)
        , on "touchstart" (Json.Decode.succeed (Down c.id))
        , on "touchend" (Json.Decode.succeed (Up c.id))
        ]
        []
