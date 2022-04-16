namespace Snake.Tests

open NUnit.Framework
open FsUnit
open Snake.Core.GameLogic
open Snake.Core.Models

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

    [<Test>]
    let ``should return snake where tail is same as head`` () =
        let initialState = getStartState 10
        initialState.Snake.Head |> should be (sameAs initialState.Snake.Tail)

    [<Test>]
    [<TestCase 1>]
    [<TestCase 3>]
    [<TestCase 5>]
    [<TestCase 10>]
    let ``should return field with height and width equal to size`` size =
        let cells = (getStartState size).Field.Cells
        
        let lengthEqualSize arr =
            arr |> should haveLength size
        
        cells |> lengthEqualSize
        cells |> Array.iter lengthEqualSize
    
    [<Test>]
    [<TestCase 1>]
    [<TestCase 3>]
    [<TestCase 5>]
    [<TestCase 10>]
    let ``should place snake at center of the field`` size =
        let head = (getStartState size).Snake.Head
        (head.X, head.Y)  |> should be (equal (size/2, size/2))

    [<Test>]
    let ``should return field with only one snake cell`` () =
        let field = (getStartState 10).Field
        let cells = field.Cells
                    |> Array.collect id
        
        let countSnakeCells counter = function
            | WithSnake _ -> counter + 1
            | _ -> counter
        
        let snakeCellCount =
            cells
            |> Array.fold countSnakeCells 0
        
        snakeCellCount |> should be (equal 1)

    [<Test>]
    [<TestCase 1>]
    [<TestCase 3>]
    [<TestCase 5>]
    [<TestCase 10>]
    let ``should return field with all none cells except one`` size =
        let field = (getStartState size).Field
        let cells = field.Cells
                    |> Array.collect id
        
        let countNoneCells counter = function
            | WithNone _ -> counter + 1
            | _ -> counter
        
        let snakeCellCount =
            cells
            |> Array.fold countNoneCells 0
        
        let noneCellCount = (size*size) - 1
        
        snakeCellCount |> should be (equal noneCellCount)
