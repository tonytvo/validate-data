module ValidateRule.UnitTest

open NUnit.Framework
open FsUnit
open ValidationResult

let validateEvenWithErrorMessage number = create (number % 2 = 0)

let validateEven number = value (validateEvenWithErrorMessage(number))
let validatePositive number = number >= 0
let validateNegative number = not <| validatePositive number
let validateOdd number = not <| validateEven(number)
let (<&>) f g = (fun x -> f x && g x)
let validateOddAndPositive = validateOdd <&> validatePositive
let validateEvenAndNegative = validateEven <&> validateNegative

[<TestFixture>]
type ``test validating even number`` () =
    
    [<Test>]  
    member _.``given even number should return true``() =
        validateEven(2) |> should be True

    [<Test>]  
    member _.``given odd number should return false``() =
        validateEven(3) |> should be False

[<TestFixture>]
type ``test validating odd number`` () =

    [<Test>]  
    member _.``given odd number should return true``() =
        validateOdd(3) |> should be True

    [<Test>]  
    member _.``given even number should return false``() =
        validateOdd(2) |> should be False

[<TestFixture>]
type ``test validating positive number`` () =
    
    [<Test>]  
    member _.``given positive number should return true``() =
        validatePositive(3) |> should be True

    [<Test>]  
    member _.``given non-positive number should return false``() =
        validatePositive(-2) |> should be False

[<TestFixture>]
type ``test validating negative number`` () =
    
    [<Test>]  
    member _.``given negative number should return true``() =
        validateNegative(-2) |> should be True

    [<Test>]  
    member _.``given non-negative number should return false``() =
        validateNegative(3) |> should be False

[<TestFixture>]
type ``test validating even negative number`` () =
    
    [<Test>]  
    member _.``given even and negative number should return true``() =
        validateEvenAndNegative(-2) |> should be True

    [<Test>]  
    member _.``given odd and negative number should return false``() =
        validateEvenAndNegative(-1) |> should be False

    [<Test>]  
    member _.``given even and positive number should return false``() =
        validateEvenAndNegative(4) |> should be False

[<TestFixture>]
type ``test validating odd positive number`` () =
    
    [<Test>]  
    member _.``given odd and positive number should return true``() =
        validateOddAndPositive(3) |> should be True

    [<Test>]  
    member _.``given odd and negative number should return false``() =
        validateOddAndPositive(-3) |> should be False

    [<Test>]  
    member _.``given even and negative number should return false``() =
        validateOddAndPositive(-4) |> should be False
