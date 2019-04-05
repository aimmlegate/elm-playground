import Browser
import Html exposing (Html, button, div, text, table, tr, th)
import Html.Events exposing (onClick)
import Random exposing (int, step)
import Time exposing (now, posixToMillis)
import Array exposing (..)
import Dict exposing (..)
import Maybe exposing (Maybe)

-- MODEL

type alias CellCoord = (Int, Int)

type FieldCell
  = Empty CellCoord
  | Mine CellCoord
  | Info (CellCoord, Int)
  | InvalidCell CellCoord

type ViewMaskCell
  = Hiden
  | Revealed
  | InvalidViewCell

type alias Field = List (List FieldCell) 

type alias ViewMask = Dict CellCoord ViewMaskCell

type alias Time = Float

type alias Model = {field: Field, viewMask: ViewMask } 

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

initialViewMaskGenerator field =
  let
      maskCellCreator cell dict =
        case cell of
          Empty (x, y) -> Dict.insert (x, y) Hiden dict 
          Mine (x, y) -> Dict.insert (x, y) Hiden dict 
          Info ((x, y), _) -> Dict.insert (x, y) Hiden dict 
          InvalidCell (x, y) -> Dict.insert (x, y) InvalidViewCell dict 
  in
    fieldFold maskCellCreator Dict.empty field

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


fieldFold fn start field =
  List.foldl (\row total -> List.foldl fn total row) start field

getCellCoord cell = 
  case cell of
    Empty (x, y) -> (x, y)
    Mine (x, y) -> (x, y)
    Info ((x, y), _) -> (x, y)
    InvalidCell (x, y) -> (x, y) -- need to fix that


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

neighborsCoordsStrict (x, y) =
  [
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

initfield = (minePlacer 10 (generateEmptyField 15) initialSeed) |> placeInfoMarkers

init : Model
init =
  { 
    field = initfield,
    viewMask = initialViewMaskGenerator(initfield)
  }

-- UPDATE

type Msg = Click CellCoord | Other 

revealCell viewMask coord =
  let
      reveal _ = Just Revealed
  in
      Dict.update coord reveal viewMask

isCellRevealled viewMask coord =
  case Dict.get coord viewMask of
      Just Revealed -> True
      _ -> False
          

dropMine field (x, y) =
  let
    placeMine cell =
      case cell of
        Empty (ix, iy) -> if (x == ix && y == iy) then Mine (x, y) else Empty (ix, iy)
        Mine (ix, iy) -> Mine (ix, iy)
        _ -> InvalidCell (-1, -1)
  in
    List.map (\row -> List.map (\cell -> placeMine cell) row) field

logTest : Model -> CellCoord -> Model
logTest model testdata =
  let 
    _ = Debug.log "testdata" testdata
  in
  model

recReveal model coord =
  let
      { field, viewMask } = model 
      revNeighCord = neighborsCoordsStrict coord
  in
      if isCellRevealled viewMask coord 
        then model
        else
          case getElement field coord of
            Just (Info ((x, y),  _)) -> { model | viewMask = revealCell viewMask (x, y)} 
            Just (Empty (x, y)) -> 
              let
                  updatedModel = { model | viewMask = revealCell viewMask (x, y)}
              in
                  List.foldl (\cellCoord md -> recReveal md cellCoord) updatedModel revNeighCord
            _ -> model
              

handleClick model coord =
  let
      { field, viewMask } = model  
  in
      case getElement field coord of
        Just (Mine (x, y)) -> { model | viewMask = revealCell viewMask (x, y) }
        Just (Info ((x, y),  _)) -> { model | viewMask = revealCell viewMask (x, y) }
        Just (Empty (x, y)) -> recReveal model (x, y)
        _ -> model
  

update : Msg -> Model -> Model
update msg model = 
  let
      { field, viewMask } = model
  in
      case msg of
        Click (x, y) -> handleClick model (x, y) 
        Other -> model

-- VIEW

render field v =
  let
      emptyCell = [text "-"]
      mineCell = [text "+"]
      hiddenCell = [text "o"]
      infoCell n = [text (String.fromInt n)]

      isCellRevealed coord =
        case Dict.get coord v of
          Just Hiden -> False
          Just Revealed -> True
          _ -> False

      renderCells cell =
        case cell of 
          Empty (x, y) -> th [onClick (Click (x, y))] (if isCellRevealed (x, y) then emptyCell else hiddenCell)
          Mine (x, y) -> th [onClick (Click (x, y))] (if isCellRevealed (x, y) then mineCell else hiddenCell)
          Info ((x, y), n) -> th [onClick (Click (x, y))] (if isCellRevealed (x, y) then (infoCell n) else hiddenCell) 
          _ -> th [] [text "error"]
      
      renderRow row = tr [] (List.map renderCells row)

  in
    table [] (List.map renderRow field)


view : Model -> Html Msg
view model = 
  let
      { field, viewMask } = model
  in
    div []
      [
        (render field viewMask)
      ]

  
main =
  Browser.sandbox { init = init, update = update, view = view }

