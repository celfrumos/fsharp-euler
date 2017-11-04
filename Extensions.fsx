(*  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.*)

#r "lib/MoreLinq.dll"
open System
open System.Collections.Generic
open System.Linq
open MoreLinq
open Microsoft.FSharp.Math
open System.Numerics

module Functions =
    let Identity = id
    let True = fun _ -> true

    let Not x = fun y -> x <> y
   

type Float = System.Double

type System.Double with
    static member IsMultipleOf mult num = 
        num % mult = 0.0
    
    static member IsDivisorOf num div =
            Float.IsMultipleOf div num

    static member IsDivisibleBy div num = Float.IsMultipleOf div num
    static member IsEven num = 
        Float.IsMultipleOf 2.0 num

    static member GetFactors filter sortmethod num =
         match num with
            | 1.0 -> [1.0]
            | 2.0 -> [1.0; 2.0]
            | 3.0 -> [1.0; 3.0]
            | _ ->  [for i in [ 1.0 .. sqrt num] do 
                         if Float.IsMultipleOf i num then
                            if filter i then
                                yield i
                                let other = num / i
                                if filter other then
                                    yield other

                    ]
                    |> List.distinct
                    |> sortmethod
    
    static member GetProperDivisors num =
        Float.GetFactors (fun x -> x <> num) Functions.Identity

    static member GetAllFactors num =
         Float.GetFactors Functions.True (List.sort) (num)

    static member IsPrime num =
         let calcIsPrime() =
            { 4.0 .. sqrt num} 
            |> Seq.tryFind(Float.IsDivisorOf num) 

         match num with
            | x when x <= 3.0 -> true
            | _ ->   match calcIsPrime() with
                           | Some x -> false   // If a value was found, it's not prime
                           | None -> true           // If no values could be found, that means it's prime
    static member Primes (count: int): seq<float>= 
        let mutable found = 0
        let mutable num = 2.0
        seq{ while found < count do                
                if Float.IsPrime num then
                    yield num
                    found <- found + 1                
                    num <- num + 1.0
        }

    static member ParseChar (ch: char) =
        ch.ToString()
        |> Double.Parse

    static member SumDigits (num: float)  =
        let str = num.ToString()
        
        str.ToCharArray()
        |> Array.sumBy Float.ParseChar 

type Int = System.Int32
type System.Int32 with
    
    
    ///**Description**
    /// * Returns true if num is divisible by mult
    ///**Parameters**
    ///  * `mult` - parameter of type `int`
    ///  * `num` - parameter of type `int`
    ///
    ///**Output Type**
    ///  * `bool`
    ///
    ///**Exceptions**
    ///
    static member IsMultipleOf mult num = num % mult = 0

    static member IsDivisibleBy div num = Int.IsMultipleOf div num

    static member IsDivisorOf num div = Int.IsMultipleOf div num
    static member IsEven num = Int.IsMultipleOf 2 num

    static member Sqrt num =
        num
        |> float
        |> sqrt
        |> int
        ///float >> sqrt >> int <| this

    static member GetFactors filter sortmethod num =
         match num with
            | 1 -> [1]
            | 2 -> [1; 2]
            | 3 -> [1; 3]
            | _ ->  [for i in [ 1 .. Int.Sqrt num ] do
                        if Int.IsMultipleOf i num then
                            if filter i then
                                yield i
                                let other = num / i
                                if filter other then
                                    yield other

                    ]
                    |> List.distinct
                    |> sortmethod

    static member GetProperDivisors num =
        Int.GetFactors (Functions.Not num) Functions.Identity num

    static member GetAllFactors num = 
        Int.GetFactors (fun a -> true) List.sort num
  
    static member IsPrime num =
        match num with
        | x when x <= 3 -> true
        | _ ->
            let root = Int.Sqrt num
            {2 .. root}
            |> Seq.exists (Int.IsDivisorOf num)
            |> not
  
    static member Primes (count: int): seq<int>= 
        let mutable found = 0
        let mutable num = 2
        seq{ while found < count do                
                if Int.IsPrime num then
                    yield num
                    found <- found + 1                
                    num <- num + 1
        }
    static member ParseChar (ch: char) =
        ch.ToString()
        |> Int32.Parse

    static member Square num = num * num

    static member SumDigits (num: int)  =
        let str = num.ToString()
        
        str.ToCharArray()
        |> Array.sumBy Int.ParseChar 

type Long = System.Int64

type System.Int64 with
    
    ///<summary>
    /// Returns true if this number is a multiple of <paramref name="mult"/> 
    ///</summary>
    ///
    static member IsMultipleOf mult num = num % mult = 0L
    
    static member IsDivisibleBy div num = Long.IsMultipleOf div num
    static member IsDivisorOf num div = Long.IsMultipleOf div num
    static member IsEven num = Long.IsMultipleOf 2L num

    static member  Sqrt (num: int64) =
        num
        |> float
        |> sqrt
        |> int64

type System.Int64 with

    static member Square (num: int64) = num * num
    member this.Square() = this * this

    static member  GetFactors filter sortmethod num =
            match num with
            | 1L -> [1L]
            | 2L -> [1L; 2L]
            | 3L -> [1L; 3L]
            | _ ->  [for i in [ 1L .. Long.Sqrt num] do
                        if Long.IsMultipleOf i num then
                            if filter i then
                                yield i
                                let other = num / i
                                if filter other then
                                    yield other

                    ]
                    |> List.distinct
                    |> sortmethod

    
    static member GetProperDivisors num =
        Long.GetFactors (fun x -> x <> num) Functions.Identity

    static member  GetAllFactors num = 
        Long.GetFactors  (fun a -> true) List.sort num
    
    static member IsPrime num =
            let root = Long.Sqrt num
            {2L .. root}
            |> Seq.exists (Long.IsDivisorOf num)
            |> not

    static member Primes (count: int): seq<int64>= 
        let mutable found = 0
        let mutable num = 2L
        seq{ 
            while found < count do                
                if Long.IsPrime num then
                    yield num
                    found <- found + 1                
                    num <- num + 1L
        }

    static member Pow (power: int64) (n: int64)  =
        let mutable prod = n
        for _ in 1L..power - 1L do
            prod <- prod * n

        prod     

    static member ParseChar (ch: char) =
        ch.ToString()
        |> Long.Parse

    static member SumDigits (num: int64)  =
        let str = num.ToString()
        
        str.ToCharArray()
        |> Array.sumBy Long.ParseChar 

// let inline parseChar<'T when 'T: (static member Parse : char -> 'T>(ch: char): 'T   =   string ch


type BigInteger with

    static member Square (num: bigint) = num * num    
    member this.Square() = this * this

    member this.Sqrt (n:bigint) =  
        if n.IsZero then 0I
        elif n > 0I then         
            let bitLength = bigint.Log(n, 2.0)
                            |> Math.Ceiling
                            |> Convert.ToInt32
                            
            let mutable root = 1I <<< (bitLength / 2)

            while (not <| bigint.isSqrt n root) do            
                root <- root + (n / root)
                root <- root / 2I
            root

        else 
            failwith "NaN"

    static member Sqrt (n:bigint) = bigint.Sqrt n

    static member isSqrt (n: bigint) (root: bigint) =    
        let lowerBound = root * root
        let upperBound = (root + 1I) * (root + 1I)
        
        (n >= lowerBound && n < upperBound)

let inline sqrt (num: bigint) = bigint.Sqrt num

let inline (%) (a: bigint) (b: bigint) = bigint.ModPow(a, bigint.One, b)

type BigInteger with
        
    static member Mod a b = bigint.ModPow(a, 1I, b)

    member this.Mod b = bigint.Mod this b

    ///**Description**
    /// * Returns true if num is divisible by mult
    ///**Parameters**
    ///  * `mult` - parameter of type `int`
    ///  * `num` - parameter of type `int`
    ///
    ///**Output Type**
    ///  * `bool`
    static member IsMultipleOf mult num = 
        num % mult = 0I

    static member IsDivisibleBy div num = bigint.IsMultipleOf div num

    static member IsDivisorOf num div = bigint.IsMultipleOf div num
    static member IsEven num = bigint.IsMultipleOf 2I num
    
    
    static member GetFactors filter sortmethod num =
        match num with
            | x when x = 1I -> [1I]
            | x when x = 2I -> [1I; 2I]
            | x when x = 3I -> [1I; 3I]
            | _ ->  
                [for i in [ 1I .. sqrt num ] do
                    if bigint.IsMultipleOf i num  && filter i then
                        yield i
                        let other = num / i
                        if filter other then
                            yield other
                ]
                |> List.distinct
                |> sortmethod

    static member GetProperDivisors num =
        bigint.GetFactors ((<>) num) Functions.Identity num

    static member GetAllFactors num = 
        bigint.GetFactors (fun a -> true) List.sort num
    
    static member IsPrime num =
        match num with
        | x when x <= 3I -> true
        | _ ->
            {2I .. sqrt num}            
            |> Seq.exists (bigint.IsDivisorOf num)
            |> not

    static member ParseChar (ch: char) =
        ch.ToString()
        |> bigint.Parse

    static member SumDigits (num: int)  =
        let str = num.ToString()
        
        str.ToCharArray()
        |> Array.sumBy bigint.ParseChar 

    static member Primes (count: int): seq<bigint>= 
        let mutable found = 0
        let mutable num = 2I
        seq{ 
            while found < count do                
                if bigint.IsPrime num then
                    yield num
                    found <- found + 1                
                    num <- num + 1I
        }

type Number = 
    | Int of int
    | Float of float
    | Long of int64
    | BigInt of bigint


// Member constraints with two type parameters
// Most often used with static type parameters in inline functions
let inline square(num : ^T when ^T : (static member (*) : ^T * ^T -> ^T)) =
    num * num

type Number with

    static member SumDigits num =
        let str = num.ToString()
        
        str.ToCharArray()
        |> Array.sumBy Long.ParseChar 

let inline ismult  num mult = //(num: Number) (mult: Number) = //(num: ^T when ^T : (static member IsMultipleOf: ^T -> ^T -> ^T) mult: ^T) =
    
    match num, mult with 
    | (Int i, Int m)       -> Int.IsMultipleOf i m
    | (Float f, Float m)   -> Float.IsMultipleOf f m
    | (Long l, Long  m)    -> Long.IsMultipleOf l m
    | (BigInt b, BigInt m) -> bigint.IsMultipleOf b m
    | _ -> failwith "Invalid Numbers"
    

let inline ismultof (num) (mult): bool =
    match num, mult with 
    | (Int i, Int m)       -> Int.IsMultipleOf i m
    | (Float f, Float m)   -> Float.IsMultipleOf f m
    | (Long l, Long m)     -> Long.IsMultipleOf l m
    | (BigInt b, BigInt m) -> bigint.IsMultipleOf b m
    | _ -> failwith "Invalid Numbers"

type IEnumerable<'T> with 
    member this.ContainsAllMembers (other: IEnumerable<'T>) =
        this = other
        || (this.All(fun x -> other.Contains x) 
            && other.All(fun y -> this.Contains y))

let GetPermutationsString (range: IEnumerable<'T>) =
        range.Permutations()
        |> Seq.map (fun x -> String.Join("", x))
