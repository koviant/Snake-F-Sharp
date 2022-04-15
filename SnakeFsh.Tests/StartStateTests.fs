namespace Snake.Tests

open NUnit.Framework
open FsUnit
open Snake.Core.GameLogic

[<TestFixture>]
module ``getStartState method`` =

    [<Test>]
    [<TestCase 1>]
    [<TestCase 3>]
    [<TestCase 5>]
    [<TestCase 10>]
    let ``should return 2D array of the passed size`` size =
        let initialState = getStartState size
        initialState.Field.Cells |> should haveLength size

    [<Test>]
    let ``should return snake of length 1`` () =
        let initialState = getStartState 10
        initialState.Snake.Body |> should haveLength 1