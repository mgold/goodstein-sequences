module Main exposing (..)

import HeredInt
import Html exposing (Html, Attribute, div, input, text, button, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)


main =
    Html.beginnerProgram
        { model = model0
        , update = update
        , view = view
        }


type alias Model =
    { content : String
    , sequences : List (List Int)
    }


model0 : Model
model0 =
    Model "10" []


type Msg
    = Change String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Change s ->
            { model | content = s }

        Submit ->
            let
                n =
                    String.toInt model.content |> Result.withDefault 3
            in
                { model | sequences = goodstein n :: model.sequences }


goodstein : Int -> List Int
goodstein i =
    let
        cutoff =
            12

        chain base int =
            if base > cutoff || int == 0 then
                []
            else
                HeredInt.fromBaseAndInt base int
                    |> HeredInt.toNextBase
                    |> HeredInt.toInt
                    |> (\x -> x - 1)
                    |> (\x -> x :: chain (base + 1) x)
    in
        i :: chain 2 i


view : Model -> Html Msg
view { content, sequences } =
    div []
        [ div []
            [ input
                [ onInput Change
                , type_ "number"
                , value content
                , autofocus True
                , Html.Attributes.min "3"
                , Html.Attributes.max "50"
                  -- any higher and you overflow the stack on Chrome/Mac
                , style
                    [ ( "width", "120px" )
                    , ( "font-size", "28px" )
                    , ( "text-align", "center" )
                    ]
                ]
                []
            , button
                [ type_ "submit"
                  --, style [ ( "visibility", "hidden" ) ]
                , onClick Submit
                ]
                [ Html.text "Goodstein me!" ]
            ]
        , div []
            (List.map viewSequence sequences)
        ]


viewSequence : List Int -> Html a
viewSequence xs =
    p [] [ Html.text <| String.join ", " (List.map toString xs) ++ "..." ]
