module ValidateRule.UnitTest

open System
open NUnit.Framework
open FsUnit
open ValidationResult
open Combinator
open ValidationInput

let isEven input =
    let number = numberValue input
    number % 2 = 0

let invalidEvenNumberErrorMessage input =
    $"{stringValue input} is not even number"

let validateEven numberInput =
    createValidationResult numberInput isEven invalidEvenNumberErrorMessage

let isPositive input =
    let number = numberValue input
    number >= 0 

let invalidPositiveNumberErrorMessage input =
    $"{stringValue input} is not positive number"
    
let validatePositive input =
    createValidationResult input isPositive invalidPositiveNumberErrorMessage

let isNegative input = not <| isPositive input
let invalidNegativeNumberErrorMessage input =
    $"{stringValue input} is not negative number"

let validateNegative input =
    createValidationResult input isNegative invalidNegativeNumberErrorMessage
    
let isOdd input =
    not <| isEven(input)

let invalidOddNumberErrorMessage numberInput =
    $"{stringValue numberInput} is not odd number"

let validateOdd input =
    createValidationResult input isOdd invalidOddNumberErrorMessage 

let (<&>) f g = (fun x -> andCombine (f x) (g x))
let validateOddAndPositive = validateOdd <&> validatePositive
let validateEvenAndNegative = validateEven <&> validateNegative

let isZero number = number = 0
let validateZero numberInput =
    createValidationResultOnPredicate (isZero (numberValue numberInput)) $"{ (numberValue numberInput) } is not zero"

let (<|>) f g = (fun x -> orCombine (f x) (g x))
let validatePositiveOrZero = validatePositive <|> validateZero

let validateEvenAndPositiveOrZero = validateEven <&> validatePositive <|> validateZero

let isEmpty str = str <> null && String.IsNullOrEmpty str 
let validateEmpty strInput =
    createValidationResultOnPredicate (isEmpty (stringValue strInput)) $"{stringValue strInput} is not empty"
let isAtLeast5Characters str = String.length str >= 5
let validateAtLeast5Characters strInput =
    createValidationResultOnPredicate (isAtLeast5Characters (stringValue strInput)) $"{stringValue strInput} is not at least 5 characters"

let validateEmptyOrAtLeast5Characters = validateEmpty <|> validateAtLeast5Characters 

[<TestFixture>]
type ``test at least 5 characters string`` () =
    
    [<Test>]  
    member _.``given string of 5 character should return valid result``() =
        validateAtLeast5Characters(createInputFromString "kajkj") |> should equal createValidResult

    [<Test>]  
    member _.``given empty string should invalid result with error message``() =
        validateAtLeast5Characters(createInputFromString "a") |> should equal (InvalidResult ["a is not at least 5 characters"])


[<TestFixture>]
type ``test empty string`` () =
    
    [<Test>]  
    member _.``given empty string should return valid result``() =
        validateEmpty(createInputFromString "") |> should equal createValidResult

    [<Test>]  
    member _.``given non-empty string should invalid result with error message``() =
        validateEmpty(createInputFromString "a") |> should equal (InvalidResult ["a is not empty"])

    [<Test>]  
    member _.``given null string should invalid result with error message``() =
        validateEmpty(createInputFromString null) |> should equal (InvalidResult [" is not empty"])

type ``test empty or atleast5characters string`` () =
    
    [<Test>]  
    member _.``given empty string should return valid result``() =
        validateEmptyOrAtLeast5Characters(createInputFromString "") |> should equal createValidResult

    [<Test>]  
    member _.``given non-empty string should invalid result with error message``() =
        validateEmptyOrAtLeast5Characters(createInputFromString "a") |> should equal (InvalidResult ["a is not empty"; "a is not at least 5 characters"])

    [<Test>]  
    member _.``given null string should invalid result with error message``() =
        validateEmptyOrAtLeast5Characters(createInputFromString null) |> should equal (InvalidResult [" is not empty"; " is not at least 5 characters"])

    [<Test>]  
    member _.``given more than 5 characters string should invalid result with error message``() =
        validateEmptyOrAtLeast5Characters(createInputFromString "kjaksj") |> should equal createValidResult


[<TestFixture>]
type ``test validating zero`` () =
    
    [<Test>]  
    member _.``given zero should return valid result``() =
        validateZero (createInputFromNumber 0) |> should equal createValidResult

    [<Test>]  
    member _.``given non-zero number should invalid result with error message``() =
        validateZero (createInputFromNumber 3) |> should equal (InvalidResult ["3 is not zero"])

[<TestFixture>]
type ``test validating even number`` () =
    
    [<Test>]  
    member _.``given even number should return true``() =
        validateEven (createInputFromNumber 2) |> should equal createValidResult

    [<Test>]  
    member _.``given odd number should return false with error message``() =
        validateEven (createInputFromNumber 3) |> should equal (InvalidResult ["3 is not even number"])

[<TestFixture>]
type ``test validating odd number`` () =

    [<Test>]  
    member _.``given odd number should return true``() =
        validateOdd (createInputFromNumber 3) |> should equal createValidResult
        
    [<Test>]  
    member _.``given even number should return false``() =
        validateOdd (createInputFromNumber 2) |> should equal (InvalidResult ["2 is not odd number"])

[<TestFixture>]
type ``test validating positive number`` () =
    
    [<Test>]  
    member _.``given positive number should return true``() =
        validatePositive (createInputFromNumber 3) |> should equal createValidResult

    [<Test>]  
    member _.``given non-positive number should return false``() =
        validatePositive (createInputFromNumber -2) |> should equal (InvalidResult ["-2 is not positive number"])

[<TestFixture>]
type ``test validating negative number`` () =
    
    [<Test>]  
    member _.``given negative number should return true``() =
        validateNegative (createInputFromNumber -2) |> should equal createValidResult

    [<Test>]  
    member _.``given non-negative number should return false with error message``() =
        validateNegative (createInputFromNumber 3) |> should equal (InvalidResult ["3 is not negative number"])

[<TestFixture>]
type ``test validating even negative number`` () =
    
    [<Test>]  
    member _.``given even and negative number should return true``() =
        validateEvenAndNegative(createInputFromNumber -2) |> should equal createValidResult

    [<Test>]  
    member _.``given odd and negative number should return false``() =
        validateEvenAndNegative(createInputFromNumber -1) |> should equal (InvalidResult ["-1 is not even number"])

    [<Test>]  
    member _.``given even and positive number should return false``() =
        validateEvenAndNegative(createInputFromNumber 4) |> should equal (InvalidResult ["4 is not negative number"])

    [<Test>]  
    member _.``given odd and positive number should return false with multiple error message``() =
        validateEvenAndNegative(createInputFromNumber 5) |> should equal (InvalidResult ["5 is not even number"; "5 is not negative number"])

[<TestFixture>]
type ``test validating odd positive number`` () =
    
    [<Test>]  
    member _.``given odd and positive number should return true``() =
        validateOddAndPositive(createInputFromNumber 3) |> should equal createValidResult

    [<Test>]  
    member _.``given odd and negative number should return false``() =
        validateOddAndPositive(createInputFromNumber -3) |> should equal (InvalidResult ["-3 is not positive number"])

    [<Test>]  
    member _.``given even and negative number should return false``() =
        validateOddAndPositive(createInputFromNumber -4) |> should equal (InvalidResult ["-4 is not odd number"; "-4 is not positive number"])

[<TestFixture>]
type ``test validating positive number or zero`` () =
    
    [<Test>]  
    member _.``given zero should return valid result``() =
        validatePositiveOrZero(createInputFromNumber 0) |> should equal createValidResult

    [<Test>]  
    member _.``given positive number should return valid result``() =
        validatePositiveOrZero(createInputFromNumber 4) |> should equal createValidResult

    [<Test>]  
    member _.``given negative number should return false with multiple error messages``() =
        validatePositiveOrZero(createInputFromNumber -3) |> should equal (InvalidResult ["-3 is not positive number"; "-3 is not zero"])

[<TestFixture>]
type ``test validating even and positive or zero`` () =
    
    [<Test>]  
    member _.``given zero, 2, 4 should return valid result``() =
        validateEvenAndPositiveOrZero(createInputFromNumber 0) |> should equal createValidResult
        validateEvenAndPositiveOrZero(createInputFromNumber 2) |> should equal createValidResult
        validateEvenAndPositiveOrZero(createInputFromNumber 4) |> should equal createValidResult

    [<Test>]  
    member _.``given -2 number should return invalid results with multiple error messages``() =
        validateEvenAndPositiveOrZero(createInputFromNumber -2) |> should equal (InvalidResult ["-2 is not positive number"; "-2 is not zero"])
