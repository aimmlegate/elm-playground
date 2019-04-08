module Field exposing
    ( CellCoord
    , Field
    , FieldCell(..)
    , fieldFold
    , getAllMines
    , getCellCoord
    , getElement
    , initializeField
    , iterateCells
    )

import Array exposing (..)
import Maybe exposing (Maybe)
import Random exposing (Seed, int, step)



-- type defs


type alias CellCoord =
    ( Int, Int )


type FieldCell
    = Empty CellCoord
    | Mine CellCoord
    | Info ( CellCoord, Int )
    | InvalidCell CellCoord


type alias Field =
    List (List FieldCell)



-- internal


neighborsCoords : CellCoord -> List CellCoord
neighborsCoords ( x, y ) =
    [ ( x + 1, y + 1 )
    , ( x - 1, y - 1 )
    , ( x + 1, y - 1 )
    , ( x - 1, y + 1 )
    , ( x, y - 1 )
    , ( x, y + 1 )
    , ( x - 1, y )
    , ( x + 1, y )
    ]


randomCoordGenerator : Int -> Seed -> ( Seed, CellCoord )
randomCoordGenerator max seed =
    let
        random =
            Random.int 0 max

        ( x, nextSeed ) =
            Random.step random seed

        ( y, returnSeed ) =
            Random.step random nextSeed
    in
    ( returnSeed, ( x, y ) )


dropMine : Field -> CellCoord -> Field
dropMine field ( x, y ) =
    let
        placeMine cell =
            case cell of
                Empty ( ix, iy ) ->
                    if x == ix && y == iy then
                        Mine ( x, y )

                    else
                        Empty ( ix, iy )

                Mine ( ix, iy ) ->
                    Mine ( ix, iy )

                _ ->
                    InvalidCell ( -1, -1 )
    in
    List.map (\row -> List.map (\cell -> placeMine cell) row) field


neighborsMines : Field -> CellCoord -> FieldCell
neighborsMines field ( x, y ) =
    let
        cellNeighborsCoord =
            neighborsCoords ( x, y )

        isNeighborMine ncoord =
            case getElement field ncoord of
                Just (Mine ( _, _ )) ->
                    True

                _ ->
                    False

        neighborMineCount =
            List.foldl
                (\c total ->
                    if isNeighborMine c then
                        total + 1

                    else
                        total
                )
                0
                cellNeighborsCoord
    in
    case neighborMineCount of
        0 ->
            Empty ( x, y )

        v ->
            Info ( ( x, y ), v )


minePlacer : Int -> Seed -> Field -> Field
minePlacer n seed field =
    let
        fieldSize =
            List.length field

        aux i fld sd =
            let
                ( nextsd, ( x, y ) ) =
                    randomCoordGenerator fieldSize sd
            in
            if i == 0 then
                fld

            else
                aux (i - 1) (dropMine fld ( x, y )) nextsd
    in
    aux n field seed


placeInfoMarkers : Field -> Field
placeInfoMarkers field =
    iterateCells
        (\cell ->
            case cell of
                Empty ( x, y ) ->
                    neighborsMines field ( x, y )

                _ ->
                    cell
        )
        field


generateEmptyField : Int -> Field
generateEmptyField size =
    let
        generateTemplateField s =
            List.repeat size (List.repeat s <| Empty ( 0, 0 ))

        attachY row x =
            List.indexedMap (\i c -> Empty ( x, i )) row

        attachX template =
            List.indexedMap (\i c -> attachY c i) template
    in
    generateTemplateField size |> attachX



-- exposing


iterateCells : (FieldCell -> FieldCell) -> Field -> Field
iterateCells fn field =
    List.map (List.map fn) field


fieldFold : (FieldCell -> a -> a) -> a -> Field -> a
fieldFold fn start field =
    List.foldl (\row total -> List.foldl fn total row) start field


getAllMines : Field -> List FieldCell
getAllMines field =
    fieldFold
        (\cell total ->
            case cell of
                Mine coord ->
                    Mine coord :: total

                _ ->
                    total
        )
        []
        field


getCellCoord : FieldCell -> CellCoord
getCellCoord cell =
    case cell of
        Empty ( x, y ) ->
            ( x, y )

        Mine ( x, y ) ->
            ( x, y )

        Info ( ( x, y ), _ ) ->
            ( x, y )

        InvalidCell ( x, y ) ->
            ( x, y )


getElement : Field -> CellCoord -> Maybe FieldCell
getElement field ( x, y ) =
    let
        fieldArray =
            Array.fromList <| List.map Array.fromList field

        row =
            Array.get x fieldArray
    in
    case row of
        Just arr ->
            Array.get y arr

        Nothing ->
            Nothing


initializeField : Int -> Int -> Seed -> Field
initializeField size mines seed =
    generateEmptyField size |> minePlacer mines seed |> placeInfoMarkers
