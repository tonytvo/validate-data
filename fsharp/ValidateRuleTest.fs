module ValidateRule.UnitTest

open NUnit.Framework
open FsUnit

let validateEven number = number % 2 = 0
let validatePositive number = number >= 0

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
    let validateOdd number = not <| validateEven(number)
    
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
    let validateNegative number = not <| validatePositive number
    
    [<Test>]  
    member _.``given negative number should return true``() =
        validateNegative(-2) |> should be True

    [<Test>]  
    member _.``given non-negative number should return false``() =
        validateNegative(3) |> should be False
