module ValidationInput
    type T =
        | NumberInput of int
        | StringInput of string
    
    let createInputFromNumber number = NumberInput number
    let numberValue numberInput =
        match numberInput with
        | NumberInput number -> number
        | _ -> failwith "todo"
