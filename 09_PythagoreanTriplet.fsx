(*
    A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,

    a^2 + b^2 = c^2
    For example, 3^2 + 4^2 = 9 + 16 = 25 = 52.

    There exists exactly one Pythagorean triplet for which a + b + c = 1000.
    Find the product abc.
*)

#load "Extensions.fsx"
module Pythag =

    open System
    open Extensions
    let square x = x * x

    type triplet = int * int * int
    let equals a b = a = b

    type Triplet =

        static member Sum ((a, b, c): triplet) =
            a + b + c

        static member Product ((a, b, c): triplet) =
            a * b * c      

        static member SumEquals num (t: triplet) =
            t
            |> Triplet.Sum
            |> equals num

        static member GetList max: triplet seq =
           seq { for m in 1 .. max do
                    for n in 1 .. m-1 do
                        let a = (square m) - (square n)
                        let b = 2 * m * n
                        let c = (square m) + (square n)
                        yield a, b, c 
               }
        
        static member GetWhereSumIs sum =
            Triplet.GetList sum
            |> Seq.tryFind (Triplet.SumEquals sum)

                
    let testSum = 1000
    let test = Triplet.GetWhereSumIs testSum
    match test with
    | Some t -> printfn "Triplet with Sum %d = %A\nProduct = %d" testSum t (Triplet.Product t)
    | None -> printfn "No match found"

    //let a, b, c = Triplet.TryGetWhereSumIs 52

