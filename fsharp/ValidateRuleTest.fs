module ValidateRule.UnitTest

open NUnit.Framework
open FsUnit
open ValidationResult
open Combinator

let isEven number = number % 2 = 0

let validateEven number =
    createValidationResultOnPredicate (isEven(number)) $"{number} is not even number"

let isPositive number = number >= 0 

let validatePositiveWithErrorMessage number =
    createValidationResultOnPredicate (isPositive(number)) $"{number} is not positive number"

let validatePositive number = booleanValue (validatePositiveWithErrorMessage number)

let isNegative number = not <| isPositive number
let validateNegative number =
    createValidationResultOnPredicate (isNegative(number)) $"{number} is not negative number"
    
let isOdd number = not <| isEven(number)
let validateOddWithErrorMessage number =
    createValidationResultOnPredicate (isOdd(number)) $"{number} is not odd number"
let (<&>) f g = (fun x -> andCombine (f(x)) (g(x)))
let validateOddAndPositiveWithErrorMessage = validateOddWithErrorMessage <&> validatePositiveWithErrorMessage
let validateEvenAndNegative number = andCombine (validateEven(number)) (validateNegative(number))

[<TestFixture>]
type ``test validating even number`` () =
    
    [<Test>]  
    member _.``given even number should return true``() =
        validateEven(2) |> should equal createValidResult

    [<Test>]  
    member _.``given odd number should return false with error message``() =
        validateEven(3) |> should equal (InvalidResult "3 is not even number")

[<TestFixture>]
type ``test validating odd number`` () =

    [<Test>]  
    member _.``given odd number should return true``() =
        validateOddWithErrorMessage(3) |> should equal createValidResult
        
    [<Test>]  
    member _.``given even number should return false``() =
        validateOddWithErrorMessage(2) |> should equal (InvalidResult "2 is not odd number")

[<TestFixture>]
type ``test validating positive number`` () =
    
    [<Test>]  
    member _.``given positive number should return true``() =
        validatePositiveWithErrorMessage(3) |> should equal (createValidResult)

    [<Test>]  
    member _.``given non-positive number should return false``() =
        validatePositiveWithErrorMessage(-2) |> should equal (InvalidResult "-2 is not positive number")

[<TestFixture>]
type ``test validating negative number`` () =
    
    [<Test>]  
    member _.``given negative number should return true``() =
        validateNegative(-2) |> should equal createValidResult

    [<Test>]  
    member _.``given non-negative number should return false with error message``() =
        validateNegative(3) |> should equal (InvalidResult "3 is not negative number")

[<TestFixture>]
type ``test validating even negative number`` () =
    
    [<Test>]  
    member _.``given even and negative number should return true``() =
        validateEvenAndNegative(-2) |> should equal (ValidResult true)

    [<Test>]  
    member _.``given odd and negative number should return false``() =
        validateEvenAndNegative(-1) |> should equal (MultipleInvalidResults ["-1 is not even number"])

    [<Test>]  
    member _.``given even and positive number should return false``() =
        validateEvenAndNegative(4) |> should equal (MultipleInvalidResults ["4 is not negative number"])

    [<Test>]  
    member _.``given odd and positive number should return false with multiple error message``() =
        validateEvenAndNegative(5) |> should equal (MultipleInvalidResults ["5 is not even number"; "5 is not negative number"])

[<TestFixture>]
type ``test validating odd positive number`` () =
    
    [<Test>]  
    member _.``given odd and positive number should return true``() =
        validateOddAndPositiveWithErrorMessage(3) |> should equal createValidResult

    [<Test>]  
    member _.``given odd and negative number should return false``() =
        validateOddAndPositiveWithErrorMessage(-3) |> should equal (MultipleInvalidResults ["-3 is not positive number"])

    [<Test>]  
    member _.``given even and negative number should return false``() =
        validateOddAndPositiveWithErrorMessage(-4) |> should equal (MultipleInvalidResults ["-4 is not odd number"; "-4 is not positive number"])
