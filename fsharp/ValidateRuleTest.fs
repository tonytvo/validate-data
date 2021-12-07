module ValidateRule.UnitTest

open System
open NUnit.Framework
open FsUnit
open ValidationResult
open Combinator
open ValidationInput

let isEven number = number % 2 = 0

let validateEven numberInput =
    createValidationResultOnPredicate (isEven (numberValue numberInput)) $"{numberValue numberInput} is not even number"

let isPositive number = number >= 0 

let validatePositive numberInput =
    createValidationResultOnPredicate (isPositive (numberValue numberInput)) $"{numberValue numberInput} is not positive number"

let isNegative number = not <| isPositive number
let validateNegative numberInput =
    createValidationResultOnPredicate (isNegative (numberValue numberInput)) $"{numberValue numberInput} is not negative number"
    
let isOdd number = not <| isEven(number)
let validateOdd numberInput =
    createValidationResultOnPredicate (isOdd (numberValue numberInput)) $"{numberValue numberInput} is not odd number"

let (<&>) f g = (fun x -> andCombine (f x) (g x))
let validateOddAndPositive number = andCombine (validateOdd (createInputFromNumber number)) (validatePositive (createInputFromNumber number))
let validateEvenAndNegative number = andCombine (validateEven (createInputFromNumber number)) (validateNegative (createInputFromNumber number))

let isZero number = number = 0
let validateZero numberInput =
    createValidationResultOnPredicate (isZero (numberValue numberInput)) $"{ (numberValue numberInput) } is not zero"

let (<|>) f g = (fun x -> orCombine (f x) (g x))
let validatePositiveOrZero number = orCombine (validatePositive (createInputFromNumber number)) (validateZero (createInputFromNumber number))

let validateEvenAndPositiveOrZero number = orCombine (andCombine (validateEven (createInputFromNumber number)) (validatePositive (createInputFromNumber number))) (validateZero (createInputFromNumber number))

let isEmpty str = str <> null && String.IsNullOrEmpty str 
let validateEmpty str =
    createValidationResultOnPredicate (isEmpty str) $"{str} is not empty"
let isAtLeast5Characters str = String.length str >= 5
let validateAtLeast5Characters str =
    createValidationResultOnPredicate (isAtLeast5Characters str) $"{str} is not at least 5 characters"

[<TestFixture>]
type ``test at least 5 characters string`` () =
    
    [<Test>]  
    member _.``given string of 5 character should return valid result``() =
        validateAtLeast5Characters("kajkj") |> should equal createValidResult

    [<Test>]  
    member _.``given empty string should invalid result with error message``() =
        validateAtLeast5Characters("a") |> should equal (InvalidResult ["a is not at least 5 characters"])


[<TestFixture>]
type ``test empty string`` () =
    
    [<Test>]  
    member _.``given empty string should return valid result``() =
        validateEmpty("") |> should equal createValidResult

    [<Test>]  
    member _.``given non-empty string should invalid result with error message``() =
        validateEmpty("a") |> should equal (InvalidResult ["a is not empty"])

    [<Test>]  
    member _.``given null string should invalid result with error message``() =
        validateEmpty(null) |> should equal (InvalidResult [" is not empty"])


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
        validateEvenAndNegative(-2) |> should equal createValidResult

    [<Test>]  
    member _.``given odd and negative number should return false``() =
        validateEvenAndNegative(-1) |> should equal (InvalidResult ["-1 is not even number"])

    [<Test>]  
    member _.``given even and positive number should return false``() =
        validateEvenAndNegative(4) |> should equal (InvalidResult ["4 is not negative number"])

    [<Test>]  
    member _.``given odd and positive number should return false with multiple error message``() =
        validateEvenAndNegative(5) |> should equal (InvalidResult ["5 is not even number"; "5 is not negative number"])

[<TestFixture>]
type ``test validating odd positive number`` () =
    
    [<Test>]  
    member _.``given odd and positive number should return true``() =
        validateOddAndPositive(3) |> should equal createValidResult

    [<Test>]  
    member _.``given odd and negative number should return false``() =
        validateOddAndPositive(-3) |> should equal (InvalidResult ["-3 is not positive number"])

    [<Test>]  
    member _.``given even and negative number should return false``() =
        validateOddAndPositive(-4) |> should equal (InvalidResult ["-4 is not odd number"; "-4 is not positive number"])

[<TestFixture>]
type ``test validating positive number or zero`` () =
    
    [<Test>]  
    member _.``given zero should return valid result``() =
        validatePositiveOrZero(0) |> should equal createValidResult

    [<Test>]  
    member _.``given positive number should return valid result``() =
        validatePositiveOrZero(4) |> should equal createValidResult

    [<Test>]  
    member _.``given negative number should return false with multiple error messages``() =
        validatePositiveOrZero(-3) |> should equal (InvalidResult ["-3 is not positive number"; "-3 is not zero"])

[<TestFixture>]
type ``test validating even and positive or zero`` () =
    
    [<Test>]  
    member _.``given zero, 2, 4 should return valid result``() =
        validateEvenAndPositiveOrZero(0) |> should equal createValidResult
        validateEvenAndPositiveOrZero(2) |> should equal createValidResult
        validateEvenAndPositiveOrZero(4) |> should equal createValidResult

    [<Test>]  
    member _.``given -2 number should return invalid results with multiple error messages``() =
        validateEvenAndPositiveOrZero(-2) |> should equal (InvalidResult ["-2 is not positive number"; "-2 is not zero"])
