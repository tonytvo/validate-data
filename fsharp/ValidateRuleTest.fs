module ValidateRule.UnitTest

open NUnit.Framework
open FsUnit
open ValidationResult
open AndCombinator

let validateEvenWithErrorMessage number =
    if number % 2 = 0 then create true else createWithErrorMessage $"{number} is not even number"

let validateEven number = value (validateEvenWithErrorMessage(number))
let validatePositiveWithErrorMessage number = create (number >= 0)
let validatePositive number = value (validatePositiveWithErrorMessage number)
let validateNegative number = not <| validatePositive number
let validateNegativeWithErrorMessage number =
    if validateNegative number
    then create true
    else createWithErrorMessage $"{number} is not negative number"
    
let validateOdd number = not <| value (validateEvenWithErrorMessage(number))
let (<&>) f g = (fun x -> f x && g x)
let validateOddAndPositive = validateOdd <&> validatePositive
let validateEvenAndNegative number = value (combine (validateEvenWithErrorMessage(number)) (validateNegativeWithErrorMessage(number)))

[<TestFixture>]
type ``test validating even number`` () =
    
    [<Test>]  
    member _.``given even number should return true``() =
        validateEvenWithErrorMessage(2) |> should equal (create true)

    [<Test>]  
    member _.``given odd number should return false with error message``() =
        validateEvenWithErrorMessage(3) |> should equal (InvalidResult "3 is not even number")

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
        validatePositiveWithErrorMessage(3) |> should equal (create true)

    [<Test>]  
    member _.``given non-positive number should return false``() =
        validatePositiveWithErrorMessage(-2) |> should equal (create false)

[<TestFixture>]
type ``test validating negative number`` () =
    
    [<Test>]  
    member _.``given negative number should return true``() =
        validateNegative(-2) |> should be True

    [<Test>]  
    member _.``given non-negative number should return false with error message``() =
        validateNegativeWithErrorMessage(3) |> should equal (InvalidResult "3 is not negative number")

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
