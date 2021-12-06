module ValidationResult
    type T = ValidationResult of bool
    
    let create (result:bool) = (ValidationResult result) 
    
    let value (ValidationResult result) = result;
