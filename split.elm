module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import List exposing (indexedMap)
import String exposing (toList)

type alias Model =
    { word : List(String)}


-- type Cell
--     = Empty
    -- | Full Int


initialModel : Model
initialModel =
    { word = ["cat"]
        -- [ [ Full 2, Empty, Empty, Full 2 ]
        -- , [ Empty, Full 2, Empty, Empty ]
        -- , [ Empty, Empty, Empty, Empty ]
        -- , [ Empty, Empty, Empty, Empty ]
        -- ]
    }
removeCharacter : List(String) -> List(String)
removeCharacter word =

    case word of
        [] -> []
        
        x :: xs ->
            if (x == "cat") then
              x :: xs ++ [ "dog" ]
            else 
                x :: removeCharacter xs
type Msg
    = ParseWord
    | ParseWord2

update : Msg -> Model -> Model
update msg model =
    case msg of
        ParseWord ->
            { model
                | word = removeCharacter model.word
            }
        ParseWord2 ->
            { model
                | word = removeCharacter model.word
            }


view : Model -> Html Msg
view model =
    case model.word of
        [word] ->
            div
                [ ]
                [ div [] [ text (Debug.toString [word])]
                , button [ onClick ParseWord ] [ text "parse word"]
                ]
        ["cat","dog"] -> 
            div
                [ ]
                [ div [] [ text (Debug.toString ["cat", "dog"])]
                , button [ onClick ParseWord ] [ text "parse word"]
                ]
        _ -> text "nope"
       


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
