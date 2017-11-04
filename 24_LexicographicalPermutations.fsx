
(*
    A permutation is an ordered arrangement of objects. For example, 3124 is one possible permutation of the digits 1, 2, 3 and 4. 
    If all of the permutations are listed numerically or alphabetically, we call it lexicographic order. 
    
    The lexicographic permutations of 0, 1 and 2 are:
        012   021   102   120   201   210

    What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?
*)

open System
#load "Extensions.fsx"
#r "lib/System.ValueTuple.dll"
#r "lib/MoreLinq.dll"
open System.Linq
open MoreLinq
open Extensions

let test() = GetPermutationsString [0..9]

let getNthPerm n =
    let range = test().Take n
    range.LastOrDefault()

    
let nthVal = getNthPerm 1_000_000 // answer = 2783915460

printfn "Result:\n%A" nthVal