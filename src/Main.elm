module Main exposing (init, initialSeed, main, update, view)

import Array exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Control exposing (handleClick)
import Field exposing (Field, initializeField)
import Html exposing (Html, button, div, table, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Maybe exposing (Maybe)
import Model exposing (..)
import Random exposing (..)
import Render exposing (renderField)
import ViewMask exposing (ViewMask, initializeViewMask)



-- MODEL


type alias Model =
    { field : Field, viewMask : ViewMask }


initialSeed =
    Random.initialSeed 213213



-- need to fix that


mineField =
    initializeField 20 100 initialSeed


init : Model
init =
    { field = mineField
    , viewMask = initializeViewMask mineField
    }



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        { field, viewMask } =
            model
    in
    case msg of
        Click ( x, y ) ->
            handleClick model ( x, y )

        Other ->
            model



-- VIEW


view : Model -> Html Msg
view model =
    let
        { field, viewMask } =
            model
    in
    Grid.container []
        [ CDN.stylesheet
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "main.css" ] []
        , renderField field viewMask
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
