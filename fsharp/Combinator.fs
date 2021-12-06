module Combinator
    open ValidationResult
    
    let aggregateErrors leftPredicate rightPredicate =
        let errorMessages = 
            [leftPredicate; rightPredicate]
            |> List.filter (fun x -> not <| isValid x)
            |> List.collect errorMessageFromInvalidResult
        InvalidResult errorMessages

    let andCombine leftPredicate rightPredicate =
        let isValid = isValid leftPredicate && isValid rightPredicate
        if (isValid) then createValidResult else aggregateErrors leftPredicate rightPredicate

    let orCombine leftPredicate rightPredicate =
        let isValid = isValid leftPredicate || isValid rightPredicate
        if (isValid) then createValidResult else aggregateErrors leftPredicate rightPredicate
