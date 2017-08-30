
#if INTERACTIVE
module MultiplesOf3And5 =
#else
module MultiplesOf3And5
#endif

    open Extensions
    /// For 10, should return 3, 5, 6 and 9
    
    let IsMultipleOf3Or5 num =
            Int.IsMultipleOf 3 num || Int.IsMultipleOf 5 num
                
    let GetMatchesBelowNum num =        
            [for i in 1..num - 1 do 
                if IsMultipleOf3Or5 i  then
                    yield i]
 
 
    let GetSumOfMatches num = 
        num
        |> GetMatchesBelowNum 
        |> List.sum 
        
    let testRes = GetMatchesBelowNum 10
    
    printfn "Test result matches = %A.\nSum = %d" testRes (List.sum testRes)

    let finalRes = GetSumOfMatches 1000

    printfn "Sum of matches below 1000: %d" finalRes
