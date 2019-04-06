module Index exposing (init, initialSeed, main, update, view)

import Array exposing (..)
import Browser
import Control exposing (handleClick)
import Field exposing (Field, initializeField)
import Html exposing (Html, button, div, table, text, th, tr)
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
    initializeField 20 30 initialSeed


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
    div []
        [ renderField field viewMask
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
