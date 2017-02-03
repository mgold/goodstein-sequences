module HeredInt exposing (..)


type HeredInt
    = HeredInt Int (List Expr)


type alias Expr =
    { power : Int, coefficient : Int }


fromBaseAndInt : Int -> Int -> HeredInt
fromBaseAndInt i val =
    let
        hereditize : Int -> List Expr
        hereditize n =
            let
                power =
                    logBase (toFloat i) (toFloat n) |> floor

                coef =
                    n // (i ^ power)
            in
                Expr power coef :: []
    in
        HeredInt i (hereditize val)


toNextBase : HeredInt -> HeredInt
toNextBase (HeredInt b i) =
    HeredInt (b + 1) i


toInt : HeredInt -> Int
toInt (HeredInt base exprs) =
    let
        eval : Int -> Expr -> Int
        eval base { power, coefficient } =
            coefficient * (base ^ power)
    in
        List.map (eval base) exprs |> List.sum
