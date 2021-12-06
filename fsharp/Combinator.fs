module Combinator
    open ValidationResult
    
    let aggregateErrors leftPredicate rightPredicate =
        let errorMessages = 
            [leftPredicate; rightPredicate]
            |> List.filter (fun x -> not <| booleanValue x)
            |> List.map (fun x -> errorMessageFromSingleInvalidResult x) 
        MultipleInvalidResults errorMessages

    let andCombine leftPredicate rightPredicate =
        let isValid = booleanValue leftPredicate && booleanValue rightPredicate
        if (isValid) then (ValidResult true) else aggregateErrors leftPredicate rightPredicate

    let orCombine leftPredicate rightPredicate =
        let isValid = booleanValue leftPredicate || booleanValue rightPredicate
        if (isValid) then (ValidResult true) else aggregateErrors leftPredicate rightPredicate
