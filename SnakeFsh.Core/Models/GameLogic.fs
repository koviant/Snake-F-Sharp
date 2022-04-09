module Snake.Core.GameLogic

open Snake.Core.Models

let private getHead fieldSize = {
        Head = {
            X = fieldSize / 2
            Y = fieldSize / 2
        }
        FieldSize = fieldSize
    }

let private getDefaultState startData =
    {
        Field = {
            Cells = [
                for _ in 0..startData.FieldSize -> [
                    for _ in 0..startData.FieldSize -> Cell.WithNone
                ]
            ]
        }
        Snake = {
            Head = startData.Head
            Body = [startData.Head]
            Tail = startData.Head
        }
    }

let getStartState = getHead >> getDefaultState

let private getNewHead head direction =                  
    match direction with
    | Up -> { head with X = head.X - 1 }
    | Down -> { head with X = head.X + 1 }
    | Left -> { head with Y = head.Y - 1 }
    | Right -> { head with Y = head.Y + 1 }

let private validate field newHead =
    match field.Cells.[newHead.X].[newHead.Y] with
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
    
let createState oldState changes =
    {
        oldState with
            Snake = updateSnake oldState.Snake changes
            
    }
    
let update state direction =
    getNewHead state.Snake.Head direction
    |> validate state.Field
    |> function
    | Collided collisionType -> GameOver collisionType
    | CanGo changes -> OngoingGame <| createState state changes