//namespace Euler
///
/// A palindromic number reads the same both ways. 
/// The largest palindrome made from the product of two 2-digit numbers is 9009 = 91 × 99.
/// Find the largest palindrome made from the product of two 3-digit numbers.
///

#if INTERACTIVE
module LargestPalindrome =
#else
module LargestPalindrome
#endif
       
    open System
    open Extensions
    open System.Numerics

   
     
    let getSecondArray max min =
        let rec getIt num =
            if  num = min then
                []
            else 
               num :: getIt(num - 1)
        getIt max

    let FindLargistPalindromeBelow min max = 
        let mutable finalMax = 0
        let middle = max / 2
        let mutable x = 1
        let mutable y = 1

        for i in [ min .. max ] do
            for j in [min.. max]  do
                let prod = i * j
                if prod > finalMax && Int.IsPalindrome prod then    
                    x <- i
                    y <- j
                    finalMax <- prod

        finalMax, x, y


    //let num = 9009
    //let palindromeTest = num.IsPalindrome()
    //let isP = match palindromeTest with 
    //          | true -> "is"
    //          | false -> "is not"

    //printfn "The number %d %s a palindrome.\n" num isP

    

    let finalmax, num1, num2 = FindLargistPalindromeBelow 100 999

    printfn "Largets Palindrome = %d\nValues = %d, %d\n\n" finalmax num1 num2