import Array
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }

symbols = (Array.fromList ["-", "\\", "|", "/", "_", "+", "~"])


checkSymb : Maybe String -> String
checkSymb maybeSymb =
  case maybeSymb of
    Just value ->
      value
    Nothing ->
      "E"

getSymb : Int -> String
getSymb index =
  checkSymb (Array.get index symbols)

-- MODEL

type alias Model = { state : Int}


init : Model
init =
  { state = 0 }

-- UPDATE

type Msg = Switch

update : Msg -> Model -> Model
update msg model =
  case msg of
    Switch ->
      { model | state = modBy (Array.length symbols) (model.state + 1) }

-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Switch ] [ text "switch" ]
    , div [] [ text (getSymb model.state) ]
    ]
