//namespace Euler
///
/// The prime factors of 13195 are 5, 7, 13 and 29.    
/// What is the largest prime factor of the number 600851475143 ?
///

#load "Extensions.fsx"
open Extensions

let testNum = 600_851_475_143L
let filter a = (a <> testNum && a <> 1L && Long.IsPrime a)
let factors = Long.GetFactors filter List.sortDescending testNum
let largest = factors.Head
printfn "Largest prime factor of %d is %d" testNum largest


