module Main exposing (GameState, isLose, isWin)

import Field exposing (Field, FieldCell(..), getAllMines)
import ViewMask exposing (ViewMask, isExpoad, isFlagRight)


type GameState
    = Running
    | Win
    | Lose



-- exposing


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
