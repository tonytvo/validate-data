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
    let stringValue stringInput =
        match stringInput with
        | StringInput input -> input
        | _ -> failwith "todo"
