module ValidationResult
    type T =
        | ValidResult
        | MultipleInvalidResults of string list
    
    let createValidResult = ValidResult
    let createSingleInvalidResult (errorMessage: string) = MultipleInvalidResults [errorMessage] 
    
    let createValidationResultOnPredicate predicate errorMessage =
        if predicate
        then createValidResult
        else createSingleInvalidResult errorMessage
    
    let isValid result =
        match result with
        | ValidResult -> true
        | MultipleInvalidResults _ -> false

    let errorMessageFromSingleInvalidResult result =
        match result with
        | MultipleInvalidResults results -> results
        | _ -> failwith "todo"
