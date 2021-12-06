module ValidationResult
    type T =
        | ValidResult of bool
        | SingleInvalidResult of string
        | MultipleInvalidResults of string list
    
    let createValidResult = ValidResult true
    let createWithErrorMessage (errorMessage: string) = SingleInvalidResult errorMessage 
    
    let createValidationResultOnPredicate predicate errorMessage =
        if predicate
        then createValidResult
        else createWithErrorMessage errorMessage
    
    let booleanValue result =
        match result with
        | ValidResult result -> result
        | SingleInvalidResult _ -> false
        | MultipleInvalidResults _ -> false

    let errorMessage result =
        match result with
        | SingleInvalidResult result -> result
        | _ -> failwith "todo"
