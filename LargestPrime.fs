//namespace Euler
///
/// The prime factors of 13195 are 5, 7, 13 and 29.    
/// What is the largest prime factor of the number 600851475143 ?
///
#if INTERACTIVE
module LargestPrime =
#else
module LargestPrime 
#endif
    open System
    open System.Numerics
    open Microsoft.FSharp.Core.Operators
    open Extensions
    
    
    let testNum = 600851475143L
    let filter a = (a <> testNum && a <> 1L && Long.IsPrime a)
    let factors = Long.GetFactors filter List.sortDescending testNum
    let largest = factors.Head
    printfn "Largest prime factor of %d is %d" testNum largest
    

