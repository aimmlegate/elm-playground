module GameControl exposing (renderGameControl)

import Bootstrap.Button as Button
import Html exposing (Html, button, div, p, table, text, th, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ionicon.Android exposing (happy, sad)



-- internal


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


black : RGBA
black =
    RGBA 0 0 0 1



-- exposing


renderGameControl =
    Button.button [ Button.warning ] [ happy 30 black ]
