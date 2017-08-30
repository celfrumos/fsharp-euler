(*
    By listing the first six prime numbers: 2, 3, 5, 7, 11, and 13, we can see that the 6th prime is 13.
    What is the 10 001st prime number?
*)

#if INTERACTIVE
module NthPrime =
#else
module NthPrime
#endif

    open System
    open Extensions

    let GetNthPrime (n: int) = 
        
        let rec calc (num: int) i =             
            if i = n then 
                num
            else
                let num = num + 1
                if Int.IsPrime num then                   
                    calc num (i + 1)
                else 
                    calc num i
            
        calc 1 0
        
    let GetNPrimes (n: int) =         
        let rec calc (num: int) i =             
            if i = n then 
                []
            else
                let num = num + 1
                if Int.IsPrime num then                   
                    num::calc num (i + 1)
                else 
                    calc num i
            
        calc 1 0

    let GetNthPrimeNonRec n =
        printfn "testing"
        let mutable num = 1
        let mutable i = 0
        while i < n do
            num <- num + 1            
            if Int.IsPrime num then                
                i <- i + 1
        num

    let testN = 10001

    let testRes = GetNthPrime testN
    printfn "The %dth result is %A" testN testRes