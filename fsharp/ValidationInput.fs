module ValidationInput
    type T =
        | NumberInput of int
        | StringInput of string
    
    let createInputFromNumber number = NumberInput number
    let createInputFromString str = StringInput str
    let numberValue numberInput =
        match numberInput with
        | NumberInput number -> number
        | _ -> failwith "todo"
    let stringValue validationInput =
        match validationInput with
        | StringInput stringInput -> stringInput
        | NumberInput numberInput -> string numberInput
        | _ -> failwith "todo"
