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


type alias Model =
    { clicks : Int }



--INIT


init : flags -> ( Model, Cmd msg )
init _ =
    ( Model 0, Cmd.none )



--UPDATE


type Msg
    = Click


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Click ->
            ( { model | clicks = model.clicks + 1 }
            , Cmd.none
            )



--VIEW


view : Model -> Html Msg
view model =
    div [ onClick Click ] [ viewCircle model.clicks ]


viewCircle : Int -> Html Msg
viewCircle clicks =
    Svg.svg
        []
        [ Svg.circle
            [ SA.fill "white"
            , SA.stroke "blue"
            , SA.cx "50"
            , SA.cy "50"
            , SA.r (String.fromInt clicks)
            ]
            []
        ]
