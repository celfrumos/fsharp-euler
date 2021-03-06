﻿

///
/// Each new term in the Fibonacci sequence is generated by adding the previous two terms. 
/// By starting with 1 and 2, the first 10 terms will be:
///
/// 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...
///
/// By considering the terms in the Fibonacci sequence whose values do not exceed four million,
/// find the sum of the even-valued terms.
///
#load "Extensions.fsx"

open System
open Extensions
let MAX_NUM = 4_000_000

///
/// Get the whole fibonacci sequence below the given max valu and max count
///
let GetNthFibsBelow maxNum maxN =

    let rec getFibRec n a b =
        if (a + b < maxNum) && (n < maxN) then
            let current = a + b
            let rest = getFibRec (n + 1) b current  
            current :: rest
          else 
            [] // generated all elements - return empty list once we're done

    1::2::(getFibRec 2 1 2)


// generate list with 1, 2 and all other larger fibonaccis

let result =
    GetNthFibsBelow MAX_NUM Int32.MaxValue
    |> List.filter Int.IsEven

let sum = List.sum result
let count = List.length result

printfn "Even results: %d items found, sum = %d\n%A\n\n\n" count sum result
