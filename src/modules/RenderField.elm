module RenderField exposing (renderField)

import Bootstrap.Button as Button
import Field exposing (FieldCell(..))
import Html exposing (Html, button, div, p, table, text, th, tr)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Ionicon exposing (flag, nuclear)
import Model exposing (..)
import ViewMask
    exposing
        ( ViewMaskCell(..)
        , getViewCell
        , isCellExploaded
        , isCellRevealed
        )



-- internal


type alias RGBA =
    { red : Float
    , green : Float
    , blue : Float
    , alpha : Float
    }


black : RGBA
black =
    RGBA 0 0 0 1


red =
    RGBA 255 0 0 1


btnContainer btn =
    [ div [ class "cellbutton border" ] [ btn ] ]


renderInfo n =
    case n of
        1 ->
            p [ class "text-primary font-weight-bold" ] [ text (String.fromInt n) ]

        2 ->
            p [ class "text-success font-weight-bold" ] [ text (String.fromInt n) ]

        3 ->
            p [ class "text-warning font-weight-bold" ] [ text (String.fromInt n) ]

        _ ->
            p [ class "text-danger font-weight-bold" ] [ text (String.fromInt n) ]


renderHiddenCell viewMask ( x, y ) =
    case getViewCell viewMask ( x, y ) of
        Just MaybeMine ->
            btnContainer (Button.button [ Button.outlineSecondary ] [ flag 20 red ])

        _ ->
            btnContainer (Button.button [ Button.outlineSecondary ] [])



-- exposing


renderField field viewMask clickHandler rclickHandler =
    let
        emptyCell =
            btnContainer (Button.button [ Button.light, Button.disabled True ] [])

        mineCell =
            btnContainer (Button.button [ Button.warning, Button.disabled True ] [ nuclear 20 black ])

        exploadedCell =
            btnContainer (Button.button [ Button.danger ] [ nuclear 20 black ])

        infoCell n =
            btnContainer (Button.button [ Button.light, Button.disabled True ] [ renderInfo n ])

        renderCells cell =
            case cell of
                Empty ( x, y ) ->
                    th [ clickHandler ( x, y ), rclickHandler ( x, y ) ]
                        (if isCellRevealed viewMask ( x, y ) then
                            emptyCell

                         else
                            renderHiddenCell viewMask ( x, y )
                        )

                Mine ( x, y ) ->
                    th [ clickHandler ( x, y ), rclickHandler ( x, y ) ]
                        (if isCellRevealed viewMask ( x, y ) then
                            mineCell

                         else if isCellExploaded viewMask ( x, y ) then
                            exploadedCell

                         else
                            renderHiddenCell viewMask ( x, y )
                        )

                Info ( ( x, y ), n ) ->
                    th [ clickHandler ( x, y ), rclickHandler ( x, y ) ]
                        (if isCellRevealed viewMask ( x, y ) then
                            infoCell n

                         else
                            renderHiddenCell viewMask ( x, y )
                        )

                _ ->
                    th [] [ text "error" ]

        renderRow row =
            tr [] (List.map renderCells row)
    in
    div [ class "d-flex justify-content-center align-items-center" ] [ table [ class "border border-dark" ] (List.map renderRow field) ]
