(*
    Starting in the top left corner of a 2×2 grid, and only being able to move to the right and down, there are exactly 6 routes to the bottom right corner.
    How many such routes are there through a 20×20 grid?
*)

open System
open System.Collections.Generic
open System.Text

let getGrid rank = 
    let max = rank + 1
    let grid =
        [|
            for i in 1..max do 
                yield 
                    [| 
                        for j in 1..max do                              
                            //yield false
                            yield i * j
                    |]
                        
        |]
    grid

let getDirection vert horiz (grid: bool[][]) =
    grid.[vert].[horiz]

type Direction = 
    | Down 
    | Right


//let FoundSequences = new HashSet<direction list>()

let getLatticeNum (grid: 'T[][]) =
    let rank = grid.Length 
    rank * rank

let printGrid (grid: 'T[][]) =    
    for row in grid do
        printfn "\t%A" row

    printfn ""

let factorial n =
    let rec loop i acc =
        match i with
        | 0 | 1 -> acc
        | _ -> loop (i-1) (acc * i)
    loop n 1


// go down as far as possible
// go right as far as possible
// when neither is possible, it is finished

let findAllPaths (grid: 'T[][]) =
    let rank = grid.Length - 1
    let mutable maxDown = rank
    let mutable maxRight = rank
    let mutable count = 0

    while maxDown > 0 && maxRight > 0 do        
        for i in 0..maxDown do
            for j in i..maxRight do                
                count <- count + 1
        
            maxRight <- maxRight - 1

        maxDown <- maxDown - 1
            
    count


let testCasePasses = 
    let testGrid =  getGrid 2
    let testVal = findAllPaths testGrid

    testVal = 6, testVal

