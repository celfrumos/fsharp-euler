
(*
    Using names.txt (right click and 'Save Link/Target As...'), a 46K text file containing over five-thousand first names, begin by sorting it into alphabetical order. 
    Then working out the alphabetical value for each name, multiply this value by its alphabetical position in the list to obtain a name score.

    For example, when the list is sorted into alphabetical order, COLIN, which is worth 3 + 15 + 12 + 9 + 14 = 53, is the 938th name in the list.
    So, COLIN would obtain a score of 938 × 53 = 49714.

    What is the total of all the name scores in the file?
*)

open System;
open System.IO;
open System.Numerics;
let getNames (fileName:string) =
    use reader = new StreamReader(fileName)
    reader.ReadToEnd()

let remove (toRemove: string) (str: string) = 
    str.Replace(toRemove, "")
    
let removeChar (toRemove: char) (str: string) = 
    str.Replace(toRemove.ToString(), "")
let getAlphabetRank (ch: char) =
    match ch.ToString().ToUpper() with
    | "A" -> 1
    | "B" -> 2
    | "C" -> 3
    | "D" -> 4
    | "E" -> 5
    | "F" -> 6
    | "G" -> 7
    | "H" -> 8
    | "I" -> 9
    | "J" -> 10
    | "K" -> 11
    | "L" -> 12
    | "M" -> 13
    | "N" -> 14
    | "O" -> 15
    | "P" -> 16
    | "Q" -> 17
    | "R" -> 18
    | "S" -> 19
    | "T" -> 20
    | "U" -> 21
    | "V" -> 22
    | "W" -> 23
    | "X" -> 24
    | "Y" -> 25
    | "Z" -> 26
    | _   -> 0

let getStringRank (str: string)=
    str.ToCharArray()
    |> Array.map getAlphabetRank

let readNames() = 
    let file= getNames("names.txt")
    let names =
     file.Split(',')
     |> Array.map (removeChar '"')
     
   // printfn "%s" names.[0]
    names

let sortNames() = 
    let names=
        readNames()
        |> Array.sort

    let sorted = String.Join(",", names)
    
    //use writer = new StreamWriter("sortedNames.txt")
   // writer.Write(sorted)

    names

let getStringSum (str: string) =
    str
    |> getStringRank
    |> Array.sumBy (BigInteger)
    
let getScores() =
    let mutable i = 0
    let scores=
        sortNames()
        |> Array.map (fun str -> (str, getStringSum str))  
    
    for (name, score) in scores do
        scores.[i] <- (name, bigint.Multiply(score, BigInteger (i + 1)))
        i <- i + 1   

    scores

let sumScores()=
    getScores()
    |> Array.sumBy (fun (_, score)-> score)