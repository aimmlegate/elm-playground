module Gamplay exposing (checkGameStatus)

import Field exposing (Field, FieldCell(..), getAllMines)
import Model exposing (..)
import ViewMask exposing (ViewMask, isExpoad, isFlagRight, revealAll)


isWin : ViewMask -> Field -> Bool
isWin viewMask field =
    List.all
        (\cell ->
            case cell of
                Mine coord ->
                    isFlagRight viewMask field coord

                _ ->
                    False
        )
        (getAllMines field)


isLose : ViewMask -> Bool
isLose viewMask =
    isExpoad viewMask



-- exposing


checkGameStatus : Model -> Model
checkGameStatus model =
    let
        { field, viewMask, gameState } =
            model
    in
    case ( isLose viewMask, isWin viewMask field ) of
        ( True, _ ) ->
            { model | viewMask = revealAll viewMask, gameState = Lose }

        ( False, True ) ->
            { model | viewMask = revealAll viewMask, gameState = Win }

        _ ->
            model
