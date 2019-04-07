module Main exposing (init, initialSeed, main, update, view)

import Array exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Control exposing (handleClick, handleRightClick)
import Field exposing (Field, initializeField)
import GameControl exposing (renderGameControl)
import Html exposing (Html, button, div, table, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Maybe exposing (Maybe)
import Model exposing (..)
import Random exposing (..)
import RenderField exposing (renderField)
import ViewMask exposing (ViewMask, initializeViewMask)



-- MODEL


type alias Model =
    { field : Field, viewMask : ViewMask }


initialSeed =
    Random.initialSeed 213213



-- need to fix that


mineField =
    initializeField 5 1 initialSeed


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

        RightClick ( x, y ) ->
            handleRightClick model ( x, y )

        _ ->
            model


clickHandler coord =
    onClick (Click coord)


rclickHandler coord =
    Mouse.onContextMenu (\event -> RightClick coord)



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
        , renderGameControl
        , renderField field viewMask clickHandler rclickHandler
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
