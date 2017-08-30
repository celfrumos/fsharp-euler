(*
    Hence the difference between the sum of the squares of the first ten natural numbers and the square of the sum is 
    3025 − 385 = 2640.

    Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.
*)

#if INTERACTIVE
module SumSquares =
#else
module SumSquares
#endif
    open System
    open Extensions
    
    let Square num = num * num

    
    ///**Description**
    ///The sum of the squares of the first ten natural numbers is,
    ///  * 1^2 + 2^2 + ... + 10^2 = 385
    ///
    let GetSumOfSquares (below: int) =
        [1 .. below]
        |> List.sumBy Square 
    
    ///**Description**
    ///
    /// The square of the sum of the first ten natural numbers is,
    ///  * (1 + 2 + ... + 10)^2 = 55^2 = 3025
    ///
    let GetSquaredSum (below: int) =
        [1 .. below]
        |> List.sum
        |> Square

    let GetDifferenceBetweenSumSquaredAndSquaredSum below =
        (GetSumOfSquares below) - (GetSquaredSum below)

    let below = 100
    // for 10 should be 2640
    let sum, squares =
        (GetSumOfSquares below), (GetSquaredSum below)
    
    let diff =  squares - sum