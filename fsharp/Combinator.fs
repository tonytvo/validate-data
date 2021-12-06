module Combinator
    open ValidationResult
    
    let aggregateErrors leftPredicate rightPredicate =
        let errorMessages = 
            [leftPredicate; rightPredicate]
            |> List.filter (fun x -> not <| isValid x)
            |> List.collect errorMessageFromInvalidResult
        InvalidResult errorMessages

    let createValidationResultFromMultipleValidationResults isValid validationResult1 validationResult2 =
        if (isValid) then createValidResult else aggregateErrors validationResult1 validationResult2
        
    let andCombine leftPredicate rightPredicate =
        let isValid = isValid leftPredicate && isValid rightPredicate
        createValidationResultFromMultipleValidationResults isValid leftPredicate rightPredicate
    
    let orCombine leftPredicate rightPredicate =
        let isValid = isValid leftPredicate || isValid rightPredicate
        createValidationResultFromMultipleValidationResults isValid leftPredicate rightPredicate
