import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }


-- MODEL

type alias Model = { symb: String }

init : Model
init =
  { symb = "-" }


-- UPDATE

type Msg = Switch

update : Msg -> Model -> Model
update msg model =
  case msg of
    Switch ->
      { model | symb = "|" }


-- VIEW

view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Switch] [ text "switch" ]
    , div [] [text model.symb]
    ]
