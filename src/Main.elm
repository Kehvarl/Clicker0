module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)



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
    div [ onClick Click ] [ text ("Click me " ++ String.fromInt model.clicks) ]
