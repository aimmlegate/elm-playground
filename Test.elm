import Browser
import Html exposing (Html, button, div, text, table, tr, th)
import Html.Events exposing (onClick)

-- MODEL

type alias CellCoord = (Int, Int)

type FieldCell
  = Empty CellCoord
  | Mine CellCoord

type alias Field = List (List FieldCell) 

type alias Model = Field

generateEmptyField size = 
  let
    generateTemplateField s = List.repeat size (List.repeat s (Empty (0, 0)))
    attachY row x = List.indexedMap (\i c -> Empty(x, i) ) row
    attachX template = List.indexedMap (\i c -> attachY c i) template
  in
    generateTemplateField size |> attachX
  

init : Model
init = generateEmptyField 15

-- UPDATE

type Msg = Click CellCoord | Other

dropMine field (x, y) =
  let
    placeMine cell =
      case cell of
        Empty (ix, iy) -> if (x == ix && y == iy) then Mine (x, y) else Empty (ix, iy)
        Mine (ix, iy) -> if (x == ix && y == iy) then Empty (x, y) else Mine (ix, iy)
  in
    List.map (\row -> List.map (\cell -> placeMine cell) row) field

logTest : Model -> CellCoord -> Model
logTest model testdata =
  let 
    _ = Debug.log "testdata" testdata
  in
  model

update : Msg -> Model -> Model
update msg model = 
  case msg of
    Click (x, y) -> dropMine model (x, y)
    Other -> model

-- VIEW

render field =
  let
      renderCells cell =
        case cell of 
          Empty (x, y) -> th [onClick (Click (x, y))] [text "-"]
          Mine (x, y) -> th [] [text "+"]
      
      renderRow row = tr [] (List.map renderCells row)

  in
    table [] (List.map renderRow field)
  


view : Model -> Html Msg
view model = 
  div []
    [
      (render model)
    ]

  
main =
  Browser.sandbox { init = init, update = update, view = view }

