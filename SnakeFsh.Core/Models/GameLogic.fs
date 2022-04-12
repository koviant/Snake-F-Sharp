module Snake.Core.GameLogic

open Snake.Core.Models

let private getDefaultHead fieldSize = {
        Head = {
            X = fieldSize / 2
            Y = fieldSize / 2
        }
        FieldSize = fieldSize
    }

let private getDefaultState startData =
    let getStartSnake head = {
            Head = head
            Body = [head]
            Tail = head
        }

    let getDefaultField size = {
        Cells = [ for _ in 0..size ->
                    [ for _ in 0..size -> Cell.WithNone ] ]
        }

    { Field = getDefaultField startData.FieldSize
      Snake = getStartSnake startData.Head }

let private getUpdate field newHead =
    match field.Cells[newHead.X][newHead.Y] with
    | Border -> Collided WithBorder
    | WithSnake _ -> Collided WithSelf
    | WithFood -> CanGo { GrowSize = Grows; NewHead = newHead }
    | WithNone -> CanGo { GrowSize = StaysSameSize; NewHead = newHead }

let private updateSnake snake changes =
    let newTail = match changes.GrowSize with
                  | Grows -> snake.Tail
                  | StaysSameSize -> List.last snake.Body

    let newBody = match changes.GrowSize with
                  | Grows -> snake.Head :: snake.Body
                  | StaysSameSize -> snake.Head :: (List.take (snake.Body.Length - 1) snake.Body)

    { Head = changes.NewHead
      Tail = newTail
      Body = newBody }

let private updateField oldField updatedSnake =
    let notNewTail i j =
        i <> updatedSnake.Tail.X &&
        j <> updatedSnake.Tail.Y

    let mapSnakeCell i j cell snakePart =
        match snakePart with
        | Head dir when updatedSnake.Body.Length > 1 -> Cell.WithSnake <| Body dir
        | Head _ -> cell
        | Tail _ when notNewTail i j  -> Cell.WithNone
        | Tail _ -> cell
        | Body _ when notNewTail i j -> cell
        | Body dir -> Cell.WithSnake <| Tail dir

    {
        Cells = [ for i in 0..oldField.Cells.Length ->
                    [ for j in 0..oldField.Cells.Length ->
                        match oldField.Cells[i][j] with
                        | WithNone | WithFood | Border -> oldField.Cells[i][j]
                        | WithSnake part -> mapSnakeCell i j oldField.Cells.[i].[j] part
                    ]
                ]
    }

let generateState oldState updatedSnake = {
        Snake = updatedSnake
        Field = updateField oldState.Field updatedSnake
    } 

let private createState oldState changes =
    updateSnake oldState.Snake changes
    |> generateState oldState

[<CompiledName "GetStartState">]
let getStartState = getDefaultHead >> getDefaultState

[<CompiledName "Update">]
let update state direction =
    let getNewHead head direction =
        match direction with
        | Up -> { head with X = head.X - 1 }
        | Down -> { head with X = head.X + 1 }
        | Left -> { head with Y = head.Y - 1 }
        | Right -> { head with Y = head.Y + 1 }

    getNewHead state.Snake.Head direction
    |> getUpdate state.Field
    |> function
    | Collided collisionType -> GameOver collisionType
    | CanGo changes -> OngoingGame <| createState state changes