#load "Extensions.fsx"
open System
//open Extensions

let numToWord n =
    match n with
        | 1 -> "One"
        | 2 -> "Two"
        | 3 -> "Three"
        | 4 -> "Four"
        | 5 -> "Five"
        | 6 -> "Six"
        | 7 -> "Seven"
        | 8 -> "Eight"
        | 9 -> "Nine"
        | 10 -> "Ten"
        | 11 -> "Eleven"
        | 12 -> "Twelve"
        | 13 -> "Thirteen"
        | 14 -> "Fourteen"
        | 15 -> "Fifteen"
        | 16 -> "Sixteen"
        | 17 -> "Seventeen"
        | 18 -> "Eighteen"
        | 19 -> "Nineteen"
        | 20 -> "Twenty"
        | _ when n > 20 && n < 30 -> "Twenty-"
        | 30 -> "Thirty"
        | _ when n > 30 && n < 40 -> "Thirty-"
        | 40 -> "Fourty"
        | _ when n > 40 && n < 50 -> "Fourty-"
        | 50 -> "Fifty"
        | _ when n > 50 && n < 60 -> "Fifty-"
        | 60 -> "Sixty"
        | _ when n > 60 && n < 70 -> "Sixty-"
        | 70 -> "Seventy"
        | _ when n > 70 && n < 80 -> "Seventy-"
        | 80 -> "Eighty"
        | _ when n > 80 && n < 90 -> "Eighty-"
        | 90 -> "Ninety"
        | _ when n > 90 && n < 100 -> "Ninety-"
        | _ -> null

let rec GetLength n =    
    let length = 1
    match n with
    | _ when n >= 10 -> length + GetLength (n/10)
    | _ -> length       

let GetGroups n =
    let rec calculate num acc =
        let length = GetLength n
        let e = float(length) / 3.0 - 0.1
        let power = 
            Math.Pow(1000.0, e)
            |> int
        let group = n / power
        match length with
        | _ when length > 3 
            ->  calculate (n - group * power) ((group, int e)::acc)
        | _ 
            -> [(group, 0)]

    calculate n []  

let GetWords (num, group) =
    let rec getWords (n, g) = 
        let i = 
            Math.Pow(10.0, float(GetLength n - 1))
            |> int

        let words = []
        let AddSize = 
            match g with 
            | 3 -> " Billion" 
            | 2 -> " Million" 
            | 1 -> " Thousand" 
            | _ -> ""

        match i with
            | 100 
                -> words
                    @ [numToWord(n/i); " Hundred"] 
                    @ if n % i > 0 then 
                         [" and "] @ getWords( n % i, g)
                      else [AddSize]
            | 10 
                -> words @ [numToWord n + (if n > 20 then numToWord (n % i) else ""); AddSize]
            | 1 
                -> words @ if g = 0 then [numToWord n] else [numToWord n; AddSize]
            | _ 
                -> words

    getWords (num, group)
let Say n =
    let wordGroups = match n with
        | _ when n > 0 -> List.map GetWords (GetGroups n)
        | _ -> [["Zero"]]

    let wordList = []
    let rec buildList (groups:string list list) =
        wordList @ groups.Head 
            @ if groups.Tail.Length = 0 || groups.Tail.Head.Head = null then 
                ["."] 
            else 
                (if groups.Tail.Length = 1 && groups.Tail.Head.Length = 1 then 
                    [" and "] 
                 else [", "]) @ (buildList groups.Tail)
                 
    let wordList = buildList wordGroups

    let sentence = ""
    let rec buildSentence (words: string list) =
        sentence + words.Head 
        + if words.Tail.Length = 0 then ""
          else (buildSentence words.Tail) 

    buildSentence wordList

