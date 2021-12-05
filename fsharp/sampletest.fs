module SampleHelloWorld.UnitTest

open NUnit.Framework
open FsUnit

[<TestFixture>]
type ``sample test fixture`` () =
    
    [<Test>]  
    member _.``sample testcase``() =
        true |> should be False
