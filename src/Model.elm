module Model exposing (GameState(..), Model, Msg(..))

import Field exposing (CellCoord, Field)
import ViewMask exposing (ViewMask)


type GameState
    = Running
    | Win
    | Lose


type alias Model =
    { field : Field, viewMask : ViewMask, gameState : GameState }


type Msg
    = Click CellCoord
    | RightClick CellCoord
    | Other
