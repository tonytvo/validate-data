module ValidateRule.UnitTest

open NUnit.Framework
open FsUnit
open ValidationResult
open Combinator

let isEven number = number % 2 = 0

let validateEven number =
    createValidationResultOnPredicate (isEven(number)) $"{number} is not even number"

let isPositive number = number >= 0 

let validatePositive number =
    createValidationResultOnPredicate (isPositive(number)) $"{number} is not positive number"

let isNegative number = not <| isPositive number
let validateNegative number =
    createValidationResultOnPredicate (isNegative(number)) $"{number} is not negative number"
    
let isOdd number = not <| isEven(number)
let validateOdd number =
    createValidationResultOnPredicate (isOdd(number)) $"{number} is not odd number"
let (<&>) f g = (fun x -> andCombine (f(x)) (g(x)))
let validateOddAndPositive = validateOdd <&> validatePositive
let validateEvenAndNegative = validateEven <&> validateNegative

let isZero number = number = 0
let validateZero number =
    createValidationResultOnPredicate (isZero(number)) $"{number} is not zero"

let (<|>) f g = (fun x -> orCombine (f(x)) (g(x)))
let validatePositiveOrZero = validatePositive <|> validateZero

let validateEvenAndPositiveOrZero = validateEven <&> validatePositive <|> validateZero

[<TestFixture>]
type ``test validating zero`` () =
    
    [<Test>]  
    member _.``given zero should return valid result``() =
        validateZero(0) |> should equal createValidResult

    [<Test>]  
    member _.``given non-zero number should invalid result with error message``() =
        validateZero(3) |> should equal (InvalidResult ["3 is not zero"])

[<TestFixture>]
type ``test validating even number`` () =
    
    [<Test>]  
    member _.``given even number should return true``() =
        validateEven(2) |> should equal createValidResult

    [<Test>]  
    member _.``given odd number should return false with error message``() =
        validateEven(3) |> should equal (InvalidResult ["3 is not even number"])

[<TestFixture>]
type ``test validating odd number`` () =

    [<Test>]  
    member _.``given odd number should return true``() =
        validateOdd(3) |> should equal createValidResult
        
    [<Test>]  
    member _.``given even number should return false``() =
        validateOdd(2) |> should equal (InvalidResult ["2 is not odd number"])

[<TestFixture>]
type ``test validating positive number`` () =
    
    [<Test>]  
    member _.``given positive number should return true``() =
        validatePositive(3) |> should equal createValidResult

    [<Test>]  
    member _.``given non-positive number should return false``() =
        validatePositive(-2) |> should equal (InvalidResult ["-2 is not positive number"])

[<TestFixture>]
type ``test validating negative number`` () =
    
    [<Test>]  
    member _.``given negative number should return true``() =
        validateNegative(-2) |> should equal createValidResult

    [<Test>]  
    member _.``given non-negative number should return false with error message``() =
        validateNegative(3) |> should equal (InvalidResult ["3 is not negative number"])

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
