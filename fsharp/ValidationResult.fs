module ValidationResult
    type T =
        | ValidResult of bool
        | InvalidResult of string
        | MultipleInvalidResults of string list
    
    let createValidResult (result:bool) = ValidResult result
    let createWithErrorMessage (errorMessage: string) = InvalidResult errorMessage 
    
    let createValidationResultOnPredicate predicate errorMessage =
        if predicate
        then createValidResult true
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
