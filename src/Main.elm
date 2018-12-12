import String
import Array
import Debug
import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


main =
  Browser.sandbox { init = init, update = update, view = view }

symbols = (Array.fromList ["-", "\\", "|", "/", "_", "+", "~"])

-- FUNCTIONS -------------------------------------------------------------------

checkSymb : Maybe String -> String
checkSymb maybeSymb =
  case maybeSymb of
    Just value ->
      value
    Nothing ->
      "E"

checkIntArray : Maybe (Array.Array Int) -> Array.Array Int
checkIntArray maybeIntArray =
  case maybeIntArray of
    Just value ->
      value
    Nothing ->
      Array.empty

checkInt : Maybe Int -> Int
checkInt maybeInt =
  case maybeInt of
    Just value ->
      value
    Nothing ->
      0

getRow : Model -> Int -> String
getRow model idx =
  let
    row = Array.toList (checkIntArray (Array.get idx model.state))

    getSymb : Int -> String
    getSymb symbCode =
      (" " ++ (checkSymb (Array.get symbCode symbols)))

  in
    String.concat (List.map getSymb row)


addDivs : Model -> Int -> List (Html msg)
addDivs model divIdx =
  let
    symbDiv = div [] [ text (getRow model divIdx) ]
    nextIdx = divIdx - 1
  in
    if divIdx /= 0 then symbDiv :: (addDivs model nextIdx) else [ symbDiv ]

-- MODEL ----------------------------------------------------------------------

type alias Model = { state : Array.Array (Array.Array Int), pos : (Int, Int) }


init : Model
init =
  { state = Array.repeat 8 (Array.repeat 8 0), pos = (0,0) }


-- UPDATE ---------------------------------------------------------------------

type Msg = Switch
          | Left
          | Down

update : Msg -> Model -> Model
update msg model =
  case msg of
    Switch ->
      let
        row = checkIntArray (Array.get (Tuple.second model.pos) model.state)
        symbCode = checkInt (Array.get (Tuple.first model.pos) row)
        newSymbCode = (modBy (Array.length symbols) (symbCode - 1))
        newRow = Array.set (Tuple.first model.pos) newSymbCode row
        newState = Array.set (Tuple.second model.pos) newRow model.state
      in
      { model | state = newState}
    Left ->
      { model | pos = (Debug.log "pos" ((modBy 8 ((Tuple.first model.pos) - 1)), (Tuple.second model.pos))) }
    Down ->
      { model | pos = (Debug.log "pos" ((Tuple.first model.pos), (modBy 8 ((Tuple.second model.pos) - 1)))) }

-- VIEW ----------------------------------------------------------------------

view : Model -> Html Msg
view model =
  div []
     (button [ onClick Left ] [ text "<-" ]
     :: button [ onClick Switch ] [ text "Switch" ]
     :: button [ onClick Down ] [ text "v" ]
     :: (addDivs model 7))
