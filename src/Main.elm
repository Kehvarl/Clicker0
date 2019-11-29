module Main exposing (..)

import Browser
import Html exposing (..)



--MAIN


main : Program () Model msg
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


update : msg -> Model -> ( Model, Cmd msg )
update _ _ =
    ( Model 0, Cmd.none )



--VIEW


view : Model -> Html msg
view _ =
    div [] [ text "Click me" ]
