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

    let getDefaultField size =
        let getNoneCellArray _ =
            Array.create size Cell.WithNone
        
        { Cells =  Array.init size getNoneCellArray } 

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

    let mapSnakeCell i j cell = function
        | Head dir when updatedSnake.Body.Length > 1 -> Cell.WithSnake <| Body dir
        | Head _ -> cell
        | Tail _ when notNewTail i j  -> Cell.WithNone
        | Tail _ -> cell
        | Body _ when notNewTail i j -> cell
        | Body dir -> Cell.WithSnake <| Tail dir

    let mapCell i j cell =
        match cell with
        | WithNone | WithFood | Border -> cell
        | WithSnake snakePart -> mapSnakeCell i j cell snakePart

    let mapRow i = Array.mapi (mapCell i)

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