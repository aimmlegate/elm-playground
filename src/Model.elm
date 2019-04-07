module Model exposing (Msg(..))

import Field exposing (CellCoord, Field)
import ViewMask exposing (ViewMask)


type Msg
    = Click CellCoord
    | RightClick CellCoord
    | Other
