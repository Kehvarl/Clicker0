module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Svg
import Svg.Attributes as SA



--MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


type alias Circle =
    { id : Int
    , x : Int
    , y : Int
    , size : Int
    }


type alias Model =
    { circles : List Circle
    , next : Int
    }



--INIT


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model [ Circle 0 100 100 5, Circle 1 100 150 5 ] 2, Cmd.none )



--UPDATE


type Msg
    = Click Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click id ->
            let
                checkCircle c =
                    if c.id == id then
                        { c | size = c.size + 1 }

                    else
                        c
            in
            ( { model | circles = List.map checkCircle model.circles }
            , Cmd.none
            )



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
        , SA.stroke "blue"
        , SA.cx (String.fromInt c.x)
        , SA.cy (String.fromInt c.y)
        , SA.r (String.fromInt c.size)
        , onClick (Click c.id)
        ]
        []
