module Pazzle

type Grid =
  { gridWidth : int
    gridHeight : int }
  member inline x.Step =
    let step = float <| max x.gridWidth x.gridHeight
    400. / step
  member inline x.Width =
    let a = (float x.gridWidth) * x.Step
    (string a)
  member inline x.Height =
    let a = (float x.gridHeight) * x.Step
    (string a)

type Point =
  { row : float
    column : float }
  static member (-) (a,b) =
    { row = a.row - b.row; column = a.column - b.column }

type Path = Point * Point

type Element =
  | Entry of pos: Point
  | Goal of pos: Point

type Color =
  | Black
  | White
  | BackGround
  with
    member x.ToStr =
      match x with
      | Black -> "black"
      | White -> "White"
      | BackGround -> "#5d6c7e"

let inline color2str (x: ^X) =
  (^X: (member ToStr : string) x)