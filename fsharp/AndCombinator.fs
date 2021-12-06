module AndCombinator
    open ValidationResult
    
    let aggregateErrors leftPredicate rightPredicate =
        let errorMessages = 
            [leftPredicate; rightPredicate]
            |> List.filter (fun x -> not <| booleanValue x)
            |> List.map (fun x -> errorMessage x) 
        MultipleInvalidResults errorMessages

    let combine leftPredicate rightPredicate =
        let isValid = booleanValue leftPredicate && booleanValue rightPredicate
        if (isValid) then (ValidResult true) else aggregateErrors leftPredicate rightPredicate
