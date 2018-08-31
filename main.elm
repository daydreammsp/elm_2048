module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { board : List (List Cell)
    }


type Cell
    = Empty
    | Full Int


initialModel : Model
initialModel =
    { board =
        [ [ Full 2, Empty, Empty, Full 2 ]
        , [ Empty, Full 2, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty ]
        , [ Empty, Empty, Empty, Empty ]
        ]
    }


goRight : List Cell -> List Cell
goRight cells =
    -- 1. Make a pass over cell list, keep only ints
    -- 2. Call goRightSimpler
    -- 3. List Int --> List Cell
    -- 4. Pad to length 4 with Empty
    let
        fullCells =
            keepFullCells cells

        flattenedCells =
            goRightSimpler fullCells

        fooby =
            cellify flattenedCells

        result =
            padToLength fooby
    in
    result
    
goLeft : List Cell -> List Cell
goLeft cells =
    -- 1. Make a pass over cell list, keep only ints
    -- 2. Call goRightSimpler
    -- 3. List Int --> List Cell
    -- 4. Pad to length 4 with Empty
    let
        fullCells =
            keepFullCells cells

        flattenedCells =
            goLeftSimpler fullCells

        fooby =
            cellify flattenedCells

        result =
            padToLengthLeft fooby
    in
    result


goRightWholeBoard : List (List Cell) -> List (List Cell)
goRightWholeBoard xs =
    case xs of
        [] -> 
            []
        
        row1 :: rows ->
            goRight row1 :: goRightWholeBoard rows

goLeftWholeBoard : List (List Cell) -> List (List Cell)
goLeftWholeBoard xs =
   case xs of
        [] -> 
            []
        
        row1 :: rows ->
            goLeft row1 :: goLeftWholeBoard rows

-- Given a list of Cell, toss the Empty and retain the Full


keepFullCells : List Cell -> List Int
keepFullCells cells =
    case cells of
        [] ->
            []

        x :: xs ->
            case x of
                Empty ->
                    keepFullCells xs

                Full n ->
                    n :: keepFullCells xs


goLeftSimpler : List Int -> List Int
goLeftSimpler xs =
    case xs of
        [] ->
            []

        [ y ] ->
            [ y ]

        y :: z :: zs ->
            if y == z then
                (y + z) :: goLeftSimpler zs

            else
                y :: goLeftSimpler (z :: zs)


goRightSimpler : List Int -> List Int
goRightSimpler xs =
    -- 1. Reverse the list
    -- 2. Flatten to the left
    -- 3. Reverse it back
    List.reverse (goLeftSimpler (List.reverse xs))



-- TODO: collapse like ints
-- Given a list of Int, make a list of full Cell with those Int


cellify : List Int -> List Cell
cellify ints =
    case ints of
        [] ->
            []

        x :: xs ->
            Full x :: cellify xs



-- Given a list of cells that we happen to know is 4 or fewer
-- elements, pad it on the left with empty cells to length 4


padToLength : List Cell -> List Cell
padToLength xs =
    case xs of
        [] ->
            [ Empty, Empty, Empty, Empty ]

        [ a ] ->
            [ Empty, Empty, Empty, a ]

        [ a, b ] ->
            [ Empty, Empty, a, b ]

        [ a, b, c ] ->
            [ Empty, a, b, c ]

        [ a, b, c, d ] ->
            [ a, b, c, d ]

        _ ->
            Debug.todo "This should never happen"

padToLengthLeft : List Cell -> List Cell
padToLengthLeft xs =
    case xs of
        [] ->
            [ Empty, Empty, Empty, Empty ]

        [ a ] ->
            [ a, Empty, Empty, Empty ]

        [ a, b ] ->
            [ a, b, Empty, Empty ]

        [ a, b, c ] ->
            [ a, b, c, Empty ]

        [ a, b, c, d ] ->
            [ a, b, c, d ]

        _ ->
            Debug.todo "This should never happen"

type Msg
    = Right
    | Left


update : Msg -> Model -> Model
update msg model =
    case msg of
        Right ->
            { model
                | board = goRightWholeBoard model.board
            }
            
        Left ->
            { model
            | board = goLeftWholeBoard model.board
            }


view : Model -> Html Msg
view model =
    case model.board of
        [row1, row2, row3, row4] ->
            div
                [ ]
                [ div [] [ text (Debug.toString row1) ]
                , div [] [ text (Debug.toString row2) ]
                , div [] [ text (Debug.toString row3) ]
                , div [] [ text (Debug.toString row4) ]
                , button [ onClick Left ] [ text "<" ]
                , button [ onClick Right ] [ text ">" ]
                ]
        _ -> text "nope"
       


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
