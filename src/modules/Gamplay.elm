module Gamplay exposing (checkGameStatus, newGameTemplate)

import Field exposing (Field, FieldCell(..), getAllMines, initializeField)
import Model exposing (GameState(..), Model)
import Random exposing (Seed, int, step)
import ViewMask exposing (ViewMask, initializeViewMask, isExpoad, isFlagRight, revealAll)


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


newGameConstructor : Int -> Int -> Seed -> Model
newGameConstructor size mines seed =
    let
        ( curentseed, nextSeed ) =
            Random.step (Random.int 0 9999) seed

        mineField =
            initializeField size mines (Random.initialSeed curentseed)
    in
    { field = mineField
    , gameState = Running
    , viewMask = initializeViewMask mineField
    , mineCounter = mines
    , fieldSize = size
    , seed = nextSeed
    }


newGameTemplate seed =
    newGameConstructor 15 40 seed
