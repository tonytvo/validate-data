module AndCombinator
    open ValidationResult
    
    let combine leftPredicate rightPredicate =
        let isValid = booleanValue leftPredicate && booleanValue rightPredicate
        if (isValid) then (ValidResult true) else InvalidResult "is not valid and combinator"
