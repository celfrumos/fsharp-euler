#if INTERACTIVE
module Collatz =
#else
module Collatz
#endif

(*
    The following iterative sequence is defined for the set of positive integers:

        n → n/2 (n is even)
        n → 3n + 1 (n is odd)

    Using the rule above and starting with 13, we generate the following sequence:

        13 → 40 → 20 → 10 → 5 → 16 → 8 → 4 → 2 → 1
        It can be seen that this sequence (starting at 13 and finishing at 1) contains 10 terms. Although it has not been proved yet (Collatz Problem), 
        it is thought that all starting numbers finish at 1.

    Which starting number, under one million, produces the longest chain?

    NOTE: Once the chain starts the terms are allowed to go above one million.
*)

    open System
    open System.Collections.Generic
    open Extensions

    type collatz = {Start: int64; Length: int64}

    type complexCollatz = {Start: int64; Sequence: int64 list}

    let collatzify n =
        match Long.IsEven n with
        | true -> n / 2L
        | false -> 3L * n + 1L

    // let seq_cache = new Dictionary<int64, int64 list>()
    // let getCollatzSequence start = 
    //     let rec calculate n (len: int64 list) =           
    //         if seq_cache.ContainsKey(n) then
    //             let finalRes = len @ seq_cache.[n]
                
    //             if not(seq_cache.ContainsKey(start)) then
    //                 seq_cache.Add(start, finalRes)
                
    //             {Start = start; Sequence = finalRes}
    //         else
    //             match n with
    //             | 1L ->
    //                 let finalRes =  1L::len
    //                 seq_cache.Add(start, finalRes)
    //                 {Start = start; Sequence =finalRes}
    //             | _ -> calculate (collatzify n) (n :: len)

    //     let result = calculate start []
    //     seq_cache.Clear()
    //     result


    let collatz_cache = new Dictionary<int64, collatz>()

    let getCollatz start =
        let rec calculate n len =           
            if collatz_cache.ContainsKey(n) then
                let finalRes = {Start = start; Length = (len + collatz_cache.[n].Length)}
                
                if not(collatz_cache.ContainsKey(start)) then
                    collatz_cache.Add(start, finalRes)
                
                finalRes
            else
                match n with
                | 1L ->
                    let finalRes = {Start = start; Length = len}
                    collatz_cache.Add(start, finalRes)
                    finalRes
                | _ -> calculate (collatzify n) (1L + len)

        let result = calculate start 0L           
        collatz_cache.Clear()
        result
    

    let getMaxChainLengthBelow start =    
        let rec calculate n maxLen =
            match n with 
            | 1L -> maxLen
            | _ -> 
                    [maxLen; getCollatz n]
                    |> List.maxBy (fun a -> a.Length)
                    |> calculate (n - 1L) 

        calculate start {Start = 1L; Length = 1L}

    let getMaxChainLengthBelowNonRec maxStart =    
        let mutable maxLen = {Start = 1L; Length = 1L}
        for n in 1L..maxStart do
            let nextMax = getCollatz n
            if nextMax.Length > maxLen.Length then
                maxLen <- nextMax

        maxLen

    let testMax = 1_000_000L

    let start = DateTime.Now

    let testVal = getMaxChainLengthBelow testMax

    let finish = DateTime.Now

    let timeElapsed = finish - start


    printfn "Largest Chain Below %d: %A\nTook %A seconds" testMax testVal timeElapsed.TotalSeconds

    