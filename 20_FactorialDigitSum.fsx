(*
    n! means n × (n − 1) × ... × 3 × 2 × 1

    For example, 10! = 10 × 9 × ... × 3 × 2 × 1 = 3628800,
    and the sum of the digits in the number 10! is 3 + 6 + 2 + 8 + 8 + 0 + 0 = 27.

    Find the sum of the digits in the number 100!
*)
open System
open System.Numerics

    let toString ch = 
        ch.ToString()

    let parseChar (ch: char) = 
        Int32.Parse (toString ch)

    let fact (num: int) =
        let start = new bigint(num)
        let rec calculate n prod =
            if n = start then 
                prod
            else
                let nextN = n + bigint.One
                calculate (nextN) (prod * nextN )

        calculate  bigint.One bigint.One

    let sumDigits num =
        num.ToString().ToCharArray()
        |> Array.sumBy parseChar 
        
    let factSumDigits num =
        fact num
        |> sumDigits