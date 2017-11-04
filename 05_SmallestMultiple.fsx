
//
// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?
//

#load "Extensions.fsx"
open System
open Extensions

let GetSmallestMultiple (min, max) =
    let factors = List.rev <| [min .. max] 

    let rec getResult x =                
        let fulfillsAll =
            factors
            |> Seq.forall (Long.IsDivisorOf x)

        match fulfillsAll with
        | true -> x
        | false -> getResult (x + 1L)

    getResult max

let range = 1L, 20L

let smallestMultiple = GetSmallestMultiple range
    //printfn "\nSmallest multiple between %A = %d\n\n" range smallestMultiple