import Browser
import Html exposing (Html, button, div, text, table, tr, th)
import Html.Events exposing (onClick)
import Random exposing (int, step)
import Time exposing (now, posixToMillis)
import Array exposing (..)

-- MODEL

type alias CellCoord = (Int, Int)

type FieldCell
  = Empty CellCoord
  | Mine CellCoord
  | Info (CellCoord, Int)
  | InvalidCell


type alias Field = List (List FieldCell) 

type alias Model = Field 

type alias Time = Float


initialSeed = Random.initialSeed 213213

generateEmptyField size = 
  let
    generateTemplateField s = List.repeat size (List.repeat s (Empty (0, 0)))
    attachY row x = List.indexedMap (\i c -> Empty(x, i) ) row
    attachX template = List.indexedMap (\i c -> attachY c i) template
  in
    generateTemplateField size |> attachX

randomCoordGenerator max seed = 
  let
      random = Random.int 0 max
      (x, nextSeed) = Random.step random seed
      (y, returnSeed) = Random.step random nextSeed
  in
    (returnSeed, (x, y))

minePlacer n field seed =
  let
      fieldSize = List.length field
      aux i fld sd =
        let
            (nextsd, (x, y)) = randomCoordGenerator fieldSize sd
        in
            if i == 0
            then fld
            else aux (i - 1) (dropMine fld (x, y)) nextsd 
  in
      aux n field seed

iterateCells fn field =
  List.map (List.map fn) field

getCellCoord cell = 
  case cell of
    Empty (x, y) -> (x, y)
    Mine (x, y) -> (x, y)
    Info ((x, y), _) -> (x, y)
    InvalidCell -> (-1, -1) -- need to fix that


neighborsCoords (x, y) =
  [
    (x + 1, y + 1),
    (x - 1, y - 1),
    (x + 1, y - 1),
    (x - 1, y + 1),
    (x, y - 1),
    (x, y + 1), 
    (x - 1, y), 
    (x + 1, y)
  ]

getElement field (x, y) = 
  let
      fieldArray = Array.fromList (List.map Array.fromList field)
      row = Array.get x fieldArray        
  in
      case row of
          Just arr -> Array.get y arr
          Nothing -> Nothing 

neighborsMines field (x, y) =
  let
      cellNeighborsCoord = neighborsCoords (x, y)
      isNeighborMine ncoord = 
        case getElement field ncoord of
          Just (Mine (_, _)) -> True
          _ -> False
      neighborMineCount =
        List.foldl
          (\c total -> if isNeighborMine(c) then total + 1 else total) 
          0 
          cellNeighborsCoord
  in
      case neighborMineCount of
        0 -> Empty (x, y)
        v -> Info ((x, y), v)

placeInfoMarkers field =
  iterateCells 
  (\cell ->
    case cell of
      Empty (x, y) -> neighborsMines field (x, y)
      _ -> cell
  ) 
  field


init : Model
init = (minePlacer 10 (generateEmptyField 15) initialSeed) |> placeInfoMarkers

-- UPDATE

type Msg = Click CellCoord | Other 

dropMine field (x, y) =
  let
    placeMine cell =
      case cell of
        Empty (ix, iy) -> if (x == ix && y == iy) then Mine (x, y) else Empty (ix, iy)
        Mine (ix, iy) -> Mine (ix, iy)
        _ -> InvalidCell
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
          Info ((x, y), n) -> th [] [text (String.fromInt n)]
          _ -> th [] [text "error"]
      
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

