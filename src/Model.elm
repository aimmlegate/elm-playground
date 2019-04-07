module Model exposing (GameState(..), Model, Msg(..))

import Field exposing (CellCoord, Field)
import Random exposing (Seed)
import ViewMask exposing (ViewMask)


type GameState
    = Running
    | Win
    | Lose


type alias Model =
    { field : Field, viewMask : ViewMask, gameState : GameState, mineCounter : Int, fieldSize : Int, seed : Seed }


type Msg
    = Click CellCoord
    | RightClick CellCoord
    | NewGame
    | NewRandomNumber Int
