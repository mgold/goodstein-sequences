module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (intRange)
import String
import HeredInt


all : Test
all =
    describe "HeredInt"
        [ fuzz2 (intRange 2 20) (intRange 1 1000) "HeredInt roundtrips" <|
            \base i ->
                HeredInt.fromBaseAndInt base i
                    |> HeredInt.toInt
                    |> Expect.equal i
        , describe "known progressions"
            [ knownProgression [ 13, 108, 1279, 16092 ]
            ]
        ]


knownProgression : List Int -> Test
knownProgression aList =
    case aList of
        [] ->
            test "an empty progression" <| \_ -> Expect.fail "need numbers"

        x :: xs ->
            describe
                ("Progression of " ++ toString x)
                (List.indexedMap
                    (\i ( a, b ) ->
                        test (toString a ++ " -> " ++ toString b) <|
                            \_ ->
                                a
                                    |> HeredInt.fromBaseAndInt (i + 2)
                                    |> HeredInt.toNextBase
                                    |> HeredInt.toInt
                                    |> (\x -> x - 1)
                                    |> Expect.equal b
                    )
                    (List.map2 (,) aList xs)
                )
