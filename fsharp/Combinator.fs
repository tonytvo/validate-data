module Combinator
    open ValidationResult
    let isAllValid validationResult1 validationResult2 =
        [validationResult1; validationResult2] |> List.forall isValid
    let andCombine validationResult1 validationResult2 =
        createValidationResultFromMultipleValidationResults isAllValid validationResult1 validationResult2
    
    let isAnyValid validationResult1 validationResult2 =
        [validationResult1; validationResult2] |> List.exists isValid
    let orCombine validationResult1 validationResult2 =
        createValidationResultFromMultipleValidationResults isAnyValid validationResult1 validationResult2
