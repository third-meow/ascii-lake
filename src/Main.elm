import Array
import Debug
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

checkIntArray : Maybe (Array.Array Int) -> Array.Array Int
checkIntArray maybeIntArray =
  case maybeIntArray of
    Just value ->
      value
    Nothing ->
      {- if Nothing, give empty array because
         checkSymb will then catch the Nothing
         caused by 'get'-ing an empty array and
         render an "E" -}
      Array.empty

checkSymbIdx : Maybe Int -> Int
checkSymbIdx maybeInt = 
  case maybeInt of
    Just value ->
      value
    Nothing ->
      {- if Nothing, give -1, checkSymb 
         will then catch the Nothing 
         caused by 'get'-ing a negitive 
         value and render an "E" -}
      -1

getSymb : Model -> Int -> Int -> String
getSymb model x y =
  let
    row = checkIntArray (Array.get y model.state)
    symbIdx = modBy (Array.length symbols) (checkSymbIdx (Array.get x row))
  in
    checkSymb (Array.get symbIdx symbols)

addDivs : Model -> Int -> List (Html msg)
addDivs model divIdx =
  let
    x = modBy 8 divIdx
    y = divIdx // 8
    symbDiv = div [] [ text (getSymb model x y) ]
    nextIdx = divIdx - 1
  in
    if divIdx /= 0 then symbDiv :: (addDivs model nextIdx) else [ symbDiv ]

-- MODEL

type alias Model = { state : Array.Array (Array.Array Int) }


init : Model
init =
  { state = Array.fromList [ Array.fromList [0,0,0,0,0,0,0,0] ] }


-- UPDATE

type Msg = Switch

update : Msg -> Model -> Model
update msg model =
  case msg of
    Switch ->
      { model | state = model.state }

-- VIEW

view : Model -> Html Msg
view model =
  div []
     (button [ onClick Switch ] [ text "does nothing" ] :: (addDivs model 7))
