module ValidationResult
    type T =
        | ValidResult
        | InvalidResult of string list
    
    let createValidResult = ValidResult
    let createSingleInvalidResult (errorMessage: string) = InvalidResult [errorMessage] 
    
    let createValidationResultOnPredicate predicate errorMessage =
        if predicate
        then createValidResult
        else createSingleInvalidResult errorMessage
    
    let isValid result =
        match result with
        | ValidResult -> true
        | InvalidResult _ -> false

    let errorMessageFromSingleInvalidResult result =
        match result with
        | InvalidResult results -> results
        | _ -> failwith "todo"
