module Snake.Core.GameLogic

open Snake.Core.Models
open Snake.Core.Helpers.CoordinatesOperations

let private getDefaultHead fieldSize = {
        SnakeHead = {
            X = fieldSize / 2
            Y = fieldSize / 2
        }
        FieldSize = fieldSize
    }

let private getDefaultState startData =
    let getStartSnake() = {
            Head = startData.SnakeHead
            Body = [startData.SnakeHead]
            Tail = startData.SnakeHead
        }

    let getDefaultField() =
        let isSnakeHead x y =
            sameAsTuple startData.SnakeHead (x, y)
        
        let mapCell x y =
            if isSnakeHead x y then Cell.WithSnake <| Head Up else Cell.WithNone
        
        let getNoneCellArray x =
            Array.init startData.FieldSize (mapCell x)
        
        { Cells =  Array.init startData.FieldSize getNoneCellArray } 

    { Field = getDefaultField()
      Snake = getStartSnake() }

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
    let notNewTail x y =
        not <| sameAsTuple updatedSnake.Tail (x, y)

    let mapSnakeCell x y cell = function
        | Head dir when updatedSnake.Body.Length > 1 -> Cell.WithSnake <| Body dir
        | Head _ -> cell
        | Tail _ when notNewTail x y  -> Cell.WithNone
        | Tail _ -> cell
        | Body _ when notNewTail x y -> cell
        | Body dir -> Cell.WithSnake <| Tail dir

    let mapCell x y cell =
        match cell with
        | WithNone | WithFood | Border -> cell
        | WithSnake snakePart -> mapSnakeCell x y cell snakePart

    let mapRow x =
        Array.mapi (mapCell x)

    { Cells = oldField.Cells |> Array.mapi mapRow }

let generateState oldState updatedSnake = {
        Snake = updatedSnake
        Field = updateField oldState.Field updatedSnake
    } 

let private createState oldState snakeChanges =
    updateSnake oldState.Snake snakeChanges
    |> generateState oldState

[<CompiledName "GetStartState">]
let getStartState = getDefaultHead >> getDefaultState

[<CompiledName "Update">]
let update state direction =
    let getNewHead head = function
        | Up -> { head with X = head.X - 1 }
        | Down -> { head with X = head.X + 1 }
        | Left -> { head with Y = head.Y - 1 }
        | Right -> { head with Y = head.Y + 1 }

    getNewHead state.Snake.Head direction
    |> getUpdate state.Field
    |> function
    | Collided collisionType -> GameOver collisionType
    | CanGo snakeChanges -> OngoingGame <| createState state snakeChanges