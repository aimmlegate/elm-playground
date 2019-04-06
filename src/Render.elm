module Render exposing (renderField)

import Field exposing (FieldCell(..))
import Html exposing (Html, button, div, table, text, th, tr)
import Html.Events exposing (onClick)
import Model exposing (..)
import ViewMask exposing (ViewMaskCell(..), isCellRevealed)


renderField field viewMask =
    let
        emptyCell =
            [ text "-" ]

        mineCell =
            [ text "+" ]

        hiddenCell =
            [ text "o" ]

        infoCell n =
            [ text (String.fromInt n) ]

        renderCells cell =
            case cell of
                Empty ( x, y ) ->
                    th [ onClick (Click ( x, y )) ]
                        (if isCellRevealed viewMask ( x, y ) then
                            emptyCell

                         else
                            hiddenCell
                        )

                Mine ( x, y ) ->
                    th [ onClick (Click ( x, y )) ]
                        (if isCellRevealed viewMask ( x, y ) then
                            mineCell

                         else
                            hiddenCell
                        )

                Info ( ( x, y ), n ) ->
                    th [ onClick (Click ( x, y )) ]
                        (if isCellRevealed viewMask ( x, y ) then
                            infoCell n

                         else
                            hiddenCell
                        )

                _ ->
                    th [] [ text "error" ]

        renderRow row =
            tr [] (List.map renderCells row)
    in
    table [] (List.map renderRow field)
