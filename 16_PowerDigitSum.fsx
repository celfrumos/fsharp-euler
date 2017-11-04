(*
    2^15 = 32768 and the sum of its digits is 3 + 2 + 7 + 6 + 8 = 26.
    What is the sum of the digits of the number 2^1000?
*)
#load "Extensions.fsx"
open System
open System.Numerics
open Extensions

type BigInteger with    

    static member SumDigits (num: bigint) =
        let str = num.ToString()
        
        str.ToCharArray()
        |> Array.sumBy bigint.ParseChar
        

let LongPowSucceeds =
    let grid = [2L; 3L; 4L; 5L; 6L]

    let expected = [4L; 9L; 16L; 25L; 36L]

    let result = 
        grid
        |> List.map (Long.Pow 2L)

    result


let getSumOf2ToPower power =
       Long.Pow 2L power
       |> Number.SumDigits 

       
let testCasePasses =
    getSumOf2ToPower 15L = 26L

let bigIntChallangeCase = 
    bigint.Pow(2I, 1000)
    |> BigInteger.SumDigits
    

    
// let challengeCase =
//     getSumOf2ToPower 1000L