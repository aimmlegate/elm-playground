module ViewMask exposing (ViewMask, ViewMaskCell(..), fieldRevealer, flagCounter, getViewCell, initializeViewMask, isCellExploaded, isCellRevealed, isExpoad, isFlagRight, placeFlag, removeFlag, revealAll)

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


updateMask : ViewMaskCell -> ViewMask -> CellCoord -> ViewMask
updateMask newCellType viewMask coord =
    let
        new _ =
            Just newCellType
    in
    Dict.update coord new viewMask


revealCell : ViewMask -> CellCoord -> ViewMask
revealCell viewMask coord =
    updateMask Revealed viewMask coord


exploadCell : ViewMask -> CellCoord -> ViewMask
exploadCell viewMask coord =
    updateMask Exploaded viewMask coord



-- exposing


flagCounter : ViewMask -> Int
flagCounter viewMask =
    Dict.foldl
        (\_ t c ->
            case t of
                MaybeMine ->
                    c + 1

                _ ->
                    c
        )
        0
        viewMask


isExpoad : ViewMask -> Bool
isExpoad viewMask =
    let
        viewList =
            Dict.toList viewMask
    in
    List.any
        (\cell ->
            case cell of
                ( _, Exploaded ) ->
                    True

                _ ->
                    False
        )
        viewList


isFlagRight : ViewMask -> Field -> CellCoord -> Bool
isFlagRight viewMask field coord =
    case ( getViewCell viewMask coord, getElement field coord ) of
        ( Just MaybeMine, Just (Mine _) ) ->
            True

        _ ->
            False


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
    Dict.map
        (\c t ->
            case t of
                Exploaded ->
                    Exploaded

                _ ->
                    Revealed
        )
        viewMask


getViewCell : ViewMask -> CellCoord -> Maybe ViewMaskCell
getViewCell viewMask coord =
    Dict.get coord viewMask


placeFlag : ViewMask -> CellCoord -> ViewMask
placeFlag viewMask coord =
    updateMask MaybeMine viewMask coord


removeFlag : ViewMask -> CellCoord -> ViewMask
removeFlag viewMask coord =
    updateMask Hiden viewMask coord


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
