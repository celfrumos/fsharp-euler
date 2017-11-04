(*
    Problem 48 
    The series, 1^1 + 2^2 + 3^3 + ... + 10^10 = 10405071317.
    Find the last ten digits of the series, 11 + 22 + 33 + ... + 10001000.
*)


open System
open System.Numerics

let selfPower (n: int) =
    BigInteger.Pow(bigint n, n)

let sumOfSeries belowInclusive =
    let rec calculateSum i acc =
        if i = belowInclusive then
            acc + selfPower i
        else 
            calculateSum (i + 1) (acc + selfPower i)

    calculateSum 1 0I