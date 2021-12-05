module SampleHelloWorld.UnitTest

open NUnit.Framework
open FsUnit

let validateEven number = number % 2 = 0

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
