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

    let errorMessageFromInvalidResult result =
        match result with
        | InvalidResult results -> results
        | _ -> failwith "todo"

    let aggregateErrors leftPredicate rightPredicate =
        let errorMessages = 
            [leftPredicate; rightPredicate]
            |> List.filter (fun x -> not <| isValid x)
            |> List.collect errorMessageFromInvalidResult
        InvalidResult errorMessages

    let createValidationResultFromMultipleValidationResults isValid validationResult1 validationResult2 =
        if (isValid) then createValidResult else aggregateErrors validationResult1 validationResult2
