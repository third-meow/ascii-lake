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

getSymb : Model -> Int -> Int -> String
getSymb model symbX symbY = 
    Array.get ( 
        Array.get symbY model.state
        |> Maybe.withDefault (Array.fromList [])
        |> Array.get symbX
        |> Debug.log ((String.fromInt symbX) ++ " : ")
        |> Maybe.withDefault 8
    ) symbols
    |> checkSymb


addButtons : Model -> Int -> Int -> List (Html msg)
addButtons model buttIdx divIdx =
    let
        symbButt = button [] [ text (getSymb model buttIdx divIdx) ]
    in
        if buttIdx /= 0 then symbButt :: (addButtons model (buttIdx - 1) divIdx) else [ symbButt ]

addDivs : Model -> Int -> List (Html msg)
addDivs model divIdx =
  let
    symbDiv = div [] (addButtons model 7 divIdx)
  in
    if divIdx /= 0 then symbDiv :: (addDivs model (divIdx - 1)) else [ symbDiv ] 


-- MODEL ----------------------------------------------------------------------

type alias Model = { state : Array.Array (Array.Array Int) }


init : Model
init =
  { state = Array.repeat 8 (Array.repeat 8 0) }


-- UPDATE ---------------------------------------------------------------------

type Msg = Switch
          | Left
          | Right
          | Up
          | Down

update : Msg -> Model -> Model
update msg model = model

-- VIEW ----------------------------------------------------------------------

view : Model -> Html Msg
view model =
  div [] (addDivs model 7)
