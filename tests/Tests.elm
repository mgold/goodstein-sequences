module Tests exposing (..)

import Test exposing (..)
import Expect
import Fuzz exposing (intRange)
import String
import HeredInt


all : Test
all =
    describe "HeredInt"
        [ fuzz2 (intRange 2 50) (intRange 1 1000) "HeredInt roundtrips" <|
            \base i ->
                HeredInt.fromBaseAndInt base i
                    |> HeredInt.toInt
                    |> Expect.equal i
        , fuzz2 (intRange 2 50) (intRange 1 1000) "HeredInt base roundtrips" <|
            \base i ->
                HeredInt.fromBaseAndInt base i
                    |> HeredInt.base
                    |> Expect.equal base
        , describe "Known progressions"
            [ knownProgression [ 13, 108, 1279, 16092 ]
            , knownProgression [ 15, 111, 1283, 18752, 326593, 6588344, 150994943, 3524450280, 100077777775 ]
            , knownProgression [ 3, 3, 3, 2, 1, 0 ]
            , knownProgression [ 4, 26, 41, 60, 83, 109, 139, 173, 211, 253, 299, 348, 401, 458, 519, 584, 653, 726, 803, 884, 969, 1058, 1151, 1222, 1295, 1370, 1447, 1526, 1607, 1690, 1775, 1862, 1951, 2042, 2135, 2230, 2327, 2426, 2527, 2630, 2735, 2842, 2951, 3062, 3175, 3290, 3407 ]
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
