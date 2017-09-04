    (*  If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. 
    The sum of these multiples is 23.

    Find the sum of all the multiples of 3 or 5 below 1000.*)
#if INTERACTIVE
module Extensions =
#else
module Extensions
#endif
    open System
    open Microsoft.FSharp.Math
    open System.Numerics
   

    type Float =
        static member IsMultipleOf mult num = 
            num % mult = 0.0
        
        static member IsDivisorOf num div =
                Float.IsMultipleOf div num

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

        static member GetAllFactors num =
             Float.GetFactors (fun a -> true) (List.sort) (num)

        static member IsPrime num =
             let calcIsPrime() =
                seq { 4.0 .. sqrt num} 
                |> Seq.tryFind(Float.IsDivisorOf num) 

             match num with
                | x when x <= 3.0 -> true
                | _ ->   match calcIsPrime() with
                               | Some(x) -> false   // If a value was found, it's not prime
                               | None -> true           // If no values could be found, that means it's prime
       
        static member ParseChar (ch: char) =
            ch.ToString()
            |> Double.Parse

        static member SumDigits (num: float)  =
            let str = num.ToString()
            
            str.ToCharArray()
            |> Array.sumBy Float.ParseChar 


    type Int =
        
        
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

        static member IsDivisorOf num div = Int.IsMultipleOf div num
        static member IsEven num =
            Int.IsMultipleOf 2 num

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
        static member IsPalindrome (num: int) =   
            let rec calcPalindrome (str: string) (start: int) (finish: int) =
                if start >= finish then
                    true
                else if str.[start] <> str.[finish] then
                    false
                else
                    calcPalindrome str (start + 1) (finish - 1)

            let thisStr = num.ToString()
            calcPalindrome thisStr (0) (thisStr.Length - 1)

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

        static member ParseChar (ch: char) =
            ch.ToString()
            |> Int32.Parse

        static member SumDigits (num: int)  =
            let str = num.ToString()
            
            str.ToCharArray()
            |> Array.sumBy Int.ParseChar 

    type Long  =
        
        ///<summary>
        /// Returns true if this number is a multiple of <paramref name="mult"/> 
        ///</summary>
        ///
        static member IsMultipleOf mult num = num % mult = 0L
        static member IsDivisorOf num div = Long.IsMultipleOf div num
        static member IsEven num = Long.IsMultipleOf 2L num

        static member  Sqrt (num: int64) =
            num
            |> float
            |> sqrt
            |> int64

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

        
        static member  GetAllFactors num = 
            Long.GetFactors  (fun a -> true) List.sort num
        
        static member IsPrime num =
                let root = Long.Sqrt num
                {2L .. root}
                |> Seq.exists (Long.IsDivisorOf num)
                |> not

       

        static member Pow (power: int64) (n: int64)  =
            let mutable prod = n
            for i in 1L..power - 1L do
                prod <- prod * n

            prod     

        static member ParseChar (ch: char) =
            ch.ToString()
            |> Int64.Parse
    
        static member SumDigits (num: int64)  =
            let str = num.ToString()
            
            str.ToCharArray()
            |> Array.sumBy Long.ParseChar 


            
    type Number = 
        static member SumDigits num =
            let str = num.ToString()
            
            str.ToCharArray()
            |> Array.sumBy Long.ParseChar 

    // module Tester =
    //     open Extensions       
    //     let number = 30L
            
    //     let testRes = number.GetFactors (List.sortDescending, fun x -> x.IsEven)
    //     let testIsPrime = number.IsPrime()
    //     let isEven = number.IsEven
    //     printfn "Number %A is prime = %A; Is even = %A\nFactors:\n\t%A\n" number isEven testIsPrime testRes


