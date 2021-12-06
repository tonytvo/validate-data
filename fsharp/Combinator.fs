module Combinator
    open ValidationResult
        
    let andCombine leftPredicate rightPredicate =
        let isValid = isValid leftPredicate && isValid rightPredicate
        createValidationResultFromMultipleValidationResults isValid leftPredicate rightPredicate
    
    let orCombine leftPredicate rightPredicate =
        let isValid = isValid leftPredicate || isValid rightPredicate
        createValidationResultFromMultipleValidationResults isValid leftPredicate rightPredicate
