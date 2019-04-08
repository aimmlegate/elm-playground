module Model exposing (GameDifficulty(..), GameState(..), Model, Msg(..))

import Field exposing (CellCoord, Field)
import Random exposing (Seed)
import ViewMask exposing (ViewMask)


type GameState
    = Running
    | Win
    | Lose


type GameDifficulty
    = Ease
    | Normal
    | Hard


type alias Model =
    { field : Field
    , viewMask : ViewMask
    , gameState : GameState
    , mineCounter : Int
    , fieldSize : Int
    , seed : Seed
    , difficulty : GameDifficulty
    , diffSettingOpen : Bool
    }


type Msg
    = Click CellCoord
    | RightClick CellCoord
    | NewGame GameDifficulty
    | NewRandomNumber Int
    | ChangeDifficulty String
