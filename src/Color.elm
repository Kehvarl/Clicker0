module Color exposing (Color(..), nextColor, stringFromColor)

import Html


main =
    Html.text "hello"


type Color
    = Blue
    | Red
    | Green
    | Black


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
