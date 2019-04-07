module Control exposing (handleClick, handleRightClick)

import Field exposing (CellCoord, Field, FieldCell(..), getElement)
import Maybe exposing (Maybe)
import ViewMask exposing (ViewMaskCell(..), fieldRevealer, getViewCell, placeFlag)


handleClick model coord =
    let
        { field, viewMask } =
            model
    in
    case getElement field coord of
        Just (Mine ( x, y )) ->
            { model | viewMask = fieldRevealer viewMask field ( x, y ) }

        Just (Info ( ( x, y ), _ )) ->
            { model | viewMask = fieldRevealer viewMask field ( x, y ) }

        Just (Empty ( x, y )) ->
            { model | viewMask = fieldRevealer viewMask field ( x, y ) }

        _ ->
            model


handleRightClick model coord =
    let
        { field, viewMask } =
            model
    in
    case getViewCell viewMask coord of
        Just Hiden ->
            { model | viewMask = placeFlag viewMask coord }

        _ ->
            model
