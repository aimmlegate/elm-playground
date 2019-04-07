module ViewMask exposing (ViewMask, ViewMaskCell(..), fieldRevealer, getViewCell, initializeViewMask, isCellExploaded, isCellRevealed, placeFlag, revealAll)

import Dict exposing (..)
import Field exposing (CellCoord, Field, FieldCell(..), fieldFold, getElement)
import Maybe exposing (Maybe)



-- type defs


type ViewMaskCell
    = Hiden
    | Revealed
    | Exploaded
    | MaybeMine
    | InvalidViewCell


type alias ViewMask =
    Dict CellCoord ViewMaskCell


type alias FieldModel =
    { field : Field, viewMask : ViewMask }



-- internal


neighborsCoordsStrict : CellCoord -> List CellCoord
neighborsCoordsStrict ( x, y ) =
    [ ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    ]


revealCell : ViewMask -> CellCoord -> ViewMask
revealCell viewMask coord =
    let
        reveal _ =
            Just Revealed
    in
    Dict.update coord reveal viewMask


exploadCell : ViewMask -> CellCoord -> ViewMask
exploadCell viewMask coord =
    let
        expload _ =
            Just Exploaded
    in
    Dict.update coord expload viewMask



-- exposing


isCellRevealed : ViewMask -> CellCoord -> Bool
isCellRevealed viewMask coord =
    case Dict.get coord viewMask of
        Just Revealed ->
            True

        _ ->
            False


isCellExploaded : ViewMask -> CellCoord -> Bool
isCellExploaded viewMask coord =
    case Dict.get coord viewMask of
        Just Exploaded ->
            True

        _ ->
            False


initializeViewMask : Field -> ViewMask
initializeViewMask field =
    let
        maskCellCreator cell dict =
            case cell of
                Empty ( x, y ) ->
                    Dict.insert ( x, y ) Hiden dict

                Mine ( x, y ) ->
                    Dict.insert ( x, y ) Hiden dict

                Info ( ( x, y ), _ ) ->
                    Dict.insert ( x, y ) Hiden dict

                InvalidCell ( x, y ) ->
                    Dict.insert ( x, y ) InvalidViewCell dict
    in
    fieldFold maskCellCreator Dict.empty field


revealAll : ViewMask -> ViewMask
revealAll viewMask =
    Dict.map (\_ -> always Revealed) viewMask


getViewCell : ViewMask -> CellCoord -> Maybe ViewMaskCell
getViewCell viewMask coord =
    Dict.get coord viewMask


placeFlag : ViewMask -> CellCoord -> ViewMask
placeFlag viewMask coord =
    let
        flag _ =
            Just MaybeMine
    in
    Dict.update coord flag viewMask


fieldRevealer : ViewMask -> Field -> CellCoord -> ViewMask
fieldRevealer viewMask field coord =
    let
        revNeighCord =
            neighborsCoordsStrict coord
    in
    if isCellRevealed viewMask coord then
        viewMask

    else
        case getElement field coord of
            Just (Info ( ( x, y ), _ )) ->
                revealCell viewMask ( x, y )

            Just (Mine ( x, y )) ->
                exploadCell viewMask ( x, y )

            Just (Empty ( x, y )) ->
                let
                    newMask =
                        revealCell viewMask ( x, y )
                in
                List.foldl (\crd vmask -> fieldRevealer vmask field crd) newMask revNeighCord

            _ ->
                viewMask
