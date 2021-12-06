module ValidationResult
    type T =
        | ValidResult
        | SingleInvalidResult of string
        | MultipleInvalidResults of string list
    
    let createValidResult = ValidResult
    let createWithErrorMessage (errorMessage: string) = SingleInvalidResult errorMessage 
    
    let createValidationResultOnPredicate predicate errorMessage =
        if predicate
        then createValidResult
        else createWithErrorMessage errorMessage
    
    let isValid result =
        match result with
        | ValidResult -> true
        | SingleInvalidResult _ -> false
        | MultipleInvalidResults _ -> false

    let errorMessageFromSingleInvalidResult result =
        match result with
        | SingleInvalidResult result -> result
        | _ -> failwith "todo"
