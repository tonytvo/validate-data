module ValidationResult
    type T =
        | ValidResult of bool
        | InvalidResult of string
        | MultipleInvalidResults of string list
    
    let createValidResult = ValidResult true
    let createWithErrorMessage (errorMessage: string) = InvalidResult errorMessage 
    
    let createValidationResultOnPredicate predicate errorMessage =
        if predicate
        then createValidResult
        else createWithErrorMessage errorMessage
    
    let booleanValue result =
        match result with
        | ValidResult result -> result
        | InvalidResult _ -> false
        | MultipleInvalidResults _ -> false

    let errorMessage result =
        match result with
        | InvalidResult result -> result
        | _ -> failwith "todo"
