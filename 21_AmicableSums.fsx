(*
    Let d(n) be defined as the sum of proper divisors of n (numbers less than n which divide evenly into n).
    
    If d(a) = b and d(b) = a, 
    where a ≠ b, then a and b are an amicable pair and each of a and b are called amicable numbers.

    For example, the proper divisors of 220 are 1, 2, 4, 5, 10, 11, 20, 22, 44, 55 and 110; therefore d(220) = 284. 
    The proper divisors of 284 are 1, 2, 4, 71 and 142; so d(284) = 220.

    Evaluate the sum of all the amicable numbers under 10000.
*)
#load "Extensions.fsx"

open Extensions;

let sumOfProperDivisors n =
    n
    |> Int.GetProperDivisors
    |> List.sum

let isAmicablePair (a, b) =
    if a <> b then
        let dA = sumOfProperDivisors a
        let dB = sumOfProperDivisors b
        dA = b && dB = a
        
    else false    


let GetAllPairs ceiling =
    let range = [1 .. ceiling]
    range
    |> Seq.allPairs range

let unZipTuple = fun (a, _)->  a

let GetAllAmicableNumbers ceiling =
        GetAllPairs ceiling
        |> Seq.filter isAmicablePair    
        |> Seq.map unZipTuple
        |> Seq.distinct       
       
let getAmicNumsRec ceiling =    
    let pairs = 
        GetAllPairs ceiling
        |> Seq.toArray

    let rec calculate current (all: int list) =
        if current >= ceiling then 
            all
        else
            let (a, b)= pairs.[current]
            if isAmicablePair pairs.[current]  then    
                let exists =  
                    all 
                    |> Seq.contains a
                if not exists then        
                    calculate (current + 1) (a::all)
                else 
                    calculate (current + 1) all    
        
            else 
                calculate (current + 1) all

    calculate 1 []


let answer() =
    //let arr = 
        getAmicNumsRec 5000 
        |>  Seq.sum 

let asyncAns = async{
    let! ans= GetAmicableNumsAsync 5000
    let total= 
        ans
        |> Seq.sum

    printfn "Result: %d" total
}

Async.RunSynchronously asyncAns

