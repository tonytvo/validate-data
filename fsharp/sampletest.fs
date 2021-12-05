module SampleHelloWorld.UnitTest

open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``test validating even number`` () =
    [<Test>]  
    member _.``given even number should return true``() =
        let validateEven number = true
        validateEven(2) |> should be False
