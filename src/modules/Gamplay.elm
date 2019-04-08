module Gamplay exposing
    ( changeDifficulty
    , checkGameStatus
    , initialGameConstructor
    , newGameTemplate
    )

import Field exposing (Field, FieldCell(..), getAllMines, initializeField)
import Model exposing (GameDifficulty(..), GameState(..), Model)
import Random exposing (Seed, int, step)
import ViewMask
    exposing
        ( ViewMask
        , initializeViewMask
        , isExpoad
        , isFlagRight
        , revealAll
        )


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
    <|
        getAllMines field


isLose : ViewMask -> Bool
isLose viewMask =
    isExpoad viewMask


newGameConstructor : GameDifficulty -> Int -> Int -> Seed -> Model -> Model
newGameConstructor difficulty size mines seed model =
    let
        ( curentseed, nextSeed ) =
            Random.step (Random.int 0 9999) seed

        mineField =
            initializeField size mines <| Random.initialSeed curentseed
    in
    { model
        | field = mineField
        , gameState = Running
        , viewMask = initializeViewMask mineField
        , mineCounter = mines
        , fieldSize = size
        , seed = nextSeed
        , diffSettingOpen = False
        , difficulty = difficulty
    }



-- exposing


changeDifficulty model string =
    case string of
        "Easy" ->
            { model | difficulty = Ease }

        "Normal" ->
            { model | difficulty = Normal }

        "Hard" ->
            { model | difficulty = Hard }

        _ ->
            model


initialGameConstructor : Seed -> Model
initialGameConstructor initialSeed =
    let
        ( curentseed, nextSeed ) =
            Random.step (Random.int 0 9999) initialSeed

        mineField =
            initializeField 10 10 <| Random.initialSeed curentseed
    in
    { field = mineField
    , gameState = Running
    , viewMask = initializeViewMask mineField
    , mineCounter = 10
    , fieldSize = 10
    , seed = nextSeed
    , diffSettingOpen = False
    , difficulty = Ease
    }


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


newGameTemplate difficulty seed model =
    let
        newGameConstructorWDifficulty =
            newGameConstructor difficulty
    in
    case difficulty of
        Ease ->
            newGameConstructorWDifficulty 10 10 seed model

        Normal ->
            newGameConstructorWDifficulty 16 40 seed model

        Hard ->
            newGameConstructorWDifficulty 22 100 seed model
