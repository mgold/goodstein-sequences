module HeredInt exposing (..)


type HeredInt
    = HeredInt Int (List Expr)


type alias Expr =
    { power : Int, coefficient : Int }


fromBaseAndInt : Int -> Int -> HeredInt
fromBaseAndInt base val =
    let
        hereditize : Int -> List Expr
        hereditize n =
            let
                power =
                    logBase (toFloat base) (toFloat n) |> floor

                coeff =
                    n // (base ^ power)

                remainder =
                    n - (coeff * (base ^ power))

                expr =
                    Expr power coeff

                tail =
                    if remainder == 0 then
                        []
                    else
                        hereditize remainder
            in
                expr :: tail
    in
        HeredInt base (hereditize val)


toNextBase : HeredInt -> HeredInt
toNextBase (HeredInt base exprs) =
    let
        upBase : Expr -> Expr
        upBase { power, coefficient } =
            { power = power + (power // base)
            , coefficient =
                if coefficient == base then
                    coefficient + 1
                else
                    coefficient
            }
    in
        HeredInt (base + 1) (List.map upBase exprs)


toInt : HeredInt -> Int
toInt (HeredInt base exprs) =
    let
        eval : Int -> Expr -> Int
        eval base { power, coefficient } =
            coefficient * (base ^ power)
    in
        List.map (eval base) exprs |> List.sum
