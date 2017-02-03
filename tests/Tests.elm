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
            \base i -> HeredInt.fromBaseAndInt base i |> HeredInt.toInt |> Expect.equal i
        , test "13 -> 108" <|
            \_ ->
                13
                    |> HeredInt.fromBaseAndInt 2
                    |> HeredInt.toNextBase
                    |> HeredInt.toInt
                    |> (\x -> x - 1)
                    |> Expect.equal 108
        ]
