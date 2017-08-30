(*
    The sum of the primes below 10 is 2 + 3 + 5 + 7 = 17.
    Find the sum of all the primes below two million    
*)

#if INTERACTIVE
module SumOfPrimes =
#else
module SumOfPrimes
#endif
    open System
    open System.Numerics
    open Extensions


    let GetSumOfAllPrimesBelow n =
        let rec calculate x =       
            match x with
            | a when a >= n -> 0L
            | b when Long.IsPrime b -> x + calculate (x + 1L) 
            | _ -> calculate (x + 1L)

        calculate 2L
    
    let GetSumOfAllPrimesBelowNonRec n =
        let mutable sum = 0L
        let mutable x = 2L
        while x < n do
            if Long.IsPrime x then
                sum <- sum + x
            
            x <- (x + 1L)
        sum
                
    let testVal = 
        GetSumOfAllPrimesBelowNonRec 2000000L
