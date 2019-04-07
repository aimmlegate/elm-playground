module Control exposing (handleClick, handleRightClick)

import Field exposing (CellCoord, Field, FieldCell(..), getElement)
import Gamplay exposing (checkGameStatus)
import Maybe exposing (Maybe)
import ViewMask exposing (ViewMaskCell(..), fieldRevealer, getViewCell, placeFlag)


handleClick model coord =
    let
        { field, viewMask } =
            model
    in
    case getElement field coord of
        Just (Mine ( x, y )) ->
            checkGameStatus { model | viewMask = fieldRevealer viewMask field ( x, y ) }

        Just (Info ( ( x, y ), _ )) ->
            checkGameStatus { model | viewMask = fieldRevealer viewMask field ( x, y ) }

        Just (Empty ( x, y )) ->
            checkGameStatus { model | viewMask = fieldRevealer viewMask field ( x, y ) }

        _ ->
            checkGameStatus model


handleRightClick model coord =
    let
        { field, viewMask } =
            model
    in
    case getViewCell viewMask coord of
        Just Hiden ->
            checkGameStatus { model | viewMask = placeFlag viewMask coord }

        _ ->
            checkGameStatus model
