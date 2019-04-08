module Main exposing (init, main, update, view)

import Array exposing (..)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser
import Control exposing (handleClick, handleRightClick)
import Field exposing (Field, initializeField)
import GameControl exposing (renderGameControl)
import Gamplay exposing (initialGameConstructor, newGameTemplate)
import Html exposing (Html, button, div, table, text, th, tr)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Events.Extra.Mouse as Mouse
import Maybe exposing (Maybe)
import Model exposing (GameDifficulty(..), GameState(..), Model, Msg(..))
import Random exposing (..)
import RenderField exposing (renderField)
import Task
import Time exposing (Posix)
import ViewMask exposing (ViewMask, initializeViewMask)



-- MODEL


initialSeed =
    Random.initialSeed 9324432


init : Model
init =
    initialGameConstructor initialSeed



-- UPDATE


update : Msg -> Model -> Model
update msg model =
    let
        { field, viewMask, seed } =
            model
    in
    case msg of
        Click ( x, y ) ->
            handleClick model ( x, y )

        RightClick ( x, y ) ->
            handleRightClick model ( x, y )

        NewGame difficulty ->
            newGameTemplate difficulty seed model

        _ ->
            model


clickHandler coord =
    onClick (Click coord)


rclickHandler coord =
    Mouse.onContextMenu (\event -> RightClick coord)


newGameHandler difficulty =
    onClick (NewGame difficulty)



-- VIEW


view : Model -> Html Msg
view model =
    let
        { field, viewMask, gameState } =
            model
    in
    div []
        [ CDN.stylesheet
        , Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "main.css" ] []
        , Grid.container []
            [ Grid.row []
                [ Grid.col [] [ renderGameControl model newGameHandler ]
                ]
            , Grid.row []
                [ Grid.col [] [ renderField field viewMask clickHandler rclickHandler ]
                ]
            ]
        ]


main =
    Browser.sandbox { init = init, update = update, view = view }
