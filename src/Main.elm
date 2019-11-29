module Main exposing (..)

import Browser
import Html exposing (..)



--MAIN


main : Program () Model msg
main =
    Browser.element
        { init = init
        , view = view
        , update = \msg model -> ( Model 0, Cmd.none )
        , subscriptions = \model -> Sub.none
        }


type alias Model =
    { clicks : Int }



--INIT


init _ =
    ( Model 0, Cmd.none )



--VIEW


view : Model -> Html msg
view _ =
    div [] [ text "Click me" ]
