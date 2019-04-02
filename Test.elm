import Browser
import Html exposing (Html, button, div, text, table, tr, th)
import Html.Events exposing (onClick)

-- MODEL

type alias Model = List (List String)

init : Model
init = [["X","X","X","X"],["X","X","X","X"],["X","X","X","X"],["X","X","X","X"]]


-- UPDATE

expandX field =  List.map (\l -> ("X"::l)) field

expand : Model -> Model
expand field = 
  case field of 
    x::_ -> expandX(x::field)
    [] -> []

reduceX field = List.map
  (\l -> case l of 
    _::xs -> xs
    [] -> []
  )
  field

reduce : Model -> Model
reduce field = 
  case field of
    _::xs -> reduceX xs
    [] -> []

type Msg = Expand | Reduce

update : Msg -> Model -> Model
update msg model = 
  case msg of
    Expand -> expand model
    Reduce -> reduce model

-- VIEW

renderCell txt = th [] [text txt]
renderRow row = tr [] (List.map renderCell row)
renderField field = table [] (List.map renderRow field)

view : Model -> Html Msg
view model =
  div []
  [
    (renderField model),
    button [ onClick Reduce ] [ text "-" ],
    button [ onClick Expand ] [ text "+" ]
  ]

  
main =
  Browser.sandbox { init = init, update = update, view = view }

