module ValidationResult
    type T =
        | ValidResult of bool
        | InvalidResult of string
        | MultipleInvalidResults of string list
    
    let create (result:bool) = ValidResult result
    let createWithErrorMessage (errorMessage: string) = InvalidResult errorMessage 
    
    let booleanValue result =
        match result with
        | ValidResult result -> result
        | InvalidResult _ -> false
