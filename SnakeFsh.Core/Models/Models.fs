module Snake.Core.Models

type Direction =
    | Up
    | Down
    | Left
    | Right

type Coordinates = {
    X: int
    Y: int
}

type StartData = {
    Head: Coordinates
    FieldSize: int
}

type CollisionCheckResult =
    | NoCollision of NewHead: Coordinates
    | BorderCollision
    | SnakeCollision

type Snake = {
    Head: Coordinates
    Body: Coordinates list
    Tail: Coordinates
}

type SnakeCell =
    | Head of Direction : Direction
    | Body of Direction : Direction
    | Tail of Direction : Direction

type Cell =
    | WithNone
    | WithSnake of SnakeCell
    | WithFood
    | Border

type Field = {
    Cells: Cell array array
}

type CollisionType =
    | WithBorder
    | WithSelf

type OngoingState = {
    Field: Field
    Snake: Snake
}

type GameState =
    | WaitForStart
    | OngoingGame of OngoingState
    | GameOver of CollisionType

type LengthUpdate =
    | Grows
    | StaysSameSize

type SnakeChanges = {
    GrowSize: LengthUpdate
    NewHead: Coordinates
}

type Update =
    | Collided of CollisionType
    | CanGo of SnakeChanges