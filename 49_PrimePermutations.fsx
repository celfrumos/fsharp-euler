(*
    The arithmetic sequence, 1487, 4817, 8147, in which each of the terms increases by 3330, is unusual in two ways:
        (i)  each of the three terms are prime, and, 
        (ii) each of the 4-digit numbers are permutations of one another.

    There are no arithmetic sequences made up of three 1-, 2-, or 3-digit primes, exhibiting this property, but there is one other 4-digit increasing sequence.

    What 12-digit number do you form by concatenating the three terms in this sequence?
*)


#load "Extensions.fsx"
#r "System.ValueTuple.dll"
open Extensions
open System.Numerics

let isPermutationOf (x) (y) = 
    (string x).ContainsAllMembers <| string y
    
let areAllPermutations (a,b,c) =
       isPermutationOf a b
    && isPermutationOf b c 
    && isPermutationOf a c

let areAllPrime (a, b, c)=
       bigint.IsPrime a
    && bigint.IsPrime b
    && bigint.IsPrime c

let findMatch startA startInc =
    let getRes a inc =
        a, (a + inc), (a + inc + inc)

    let rec calculate x inc =
        let res = getRes x inc
        if areAllPrime res && areAllPermutations res then  
            res
        else             
            calculate (x + 1I) (inc + 1I)
    
    calculate startA startInc

let test = findMatch 1489I 3330I
