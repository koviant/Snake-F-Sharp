module Snake.Core.Helpers.CoordinatesOperations

open Snake.Core.Models

let sameAsTuple coordinate tuple =
    (coordinate.X, coordinate.Y) = tuple