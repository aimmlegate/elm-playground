module GameControl exposing (renderGameControl)

import Bootstrap.Button as Button
import Html exposing (Html, button, div, p, table, text, th, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ionicon.Android exposing (happy, sad)
import Model exposing (GameState(..), Model)
import ViewMask exposing (flagCounter)



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


renderControl handler element =
    div [ handler ] [ element ]


renderGameControlFace model newGameHandler =
    let
        { gameState } =
            model
    in
    case gameState of
        Running ->
            renderControl newGameHandler (Button.button [ Button.warning ] [ happy 30 black ])

        Win ->
            renderControl newGameHandler (Button.button [ Button.success ] [ happy 30 black ])

        Lose ->
            renderControl newGameHandler (Button.button [ Button.danger ] [ sad 30 black ])


renderMineCounter model =
    let
        { viewMask, mineCounter } =
            model
    in
    div [ class "mr-3" ] [ text ("Mines left " ++ String.fromInt (mineCounter - flagCounter viewMask)) ]



-- exposing


renderGameControl model newGameHandler =
    div [ class "d-flex justify-content-center align-items-center mb-3 mt-3" ]
        [ renderMineCounter model
        , renderGameControlFace model newGameHandler
        ]
