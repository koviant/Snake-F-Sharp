module Snake.Core.GameLogic

open Snake.Core.Models

let private getDefaultHead fieldSize = {
            Head = { X = fieldSize / 2
                     Y = fieldSize / 2 }
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

let private getNewHead head direction =
    match direction with
    | Up -> { head with X = head.X - 1 }
    | Down -> { head with X = head.X + 1 }
    | Left -> { head with Y = head.Y - 1 }
    | Right -> { head with Y = head.Y + 1 }

let private validate field newHead =
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

    { snake with Head = changes.NewHead
                 Tail = newTail
                 Body = newBody }

let private updateField oldField updatedSnake =
    let clearSnakeCells row =
        let emptyIfSnake cell =
            match cell with
            | WithSnake _ -> WithNone
            | _ -> cell
        
        row |> List.map emptyIfSnake
    
    // stub
    let setSnakeCells cells =
        [ WithNone ]
    
    oldField.Cells
    |> List.map clearSnakeCells
    |> List.map setSnakeCells
    |> fun cells -> { Cells = cells }

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
    getNewHead state.Snake.Head direction
    |> validate state.Field
    |> function
    | Collided collisionType -> GameOver collisionType
    | CanGo changes -> OngoingGame <| createState state changes