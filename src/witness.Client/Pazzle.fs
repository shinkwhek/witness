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
  member inline x.Offset =
    { gridWidth=x.gridWidth - 1; gridHeight=x.gridHeight - 1 }

type Point =
  { row : float
    column : float } 
  static member inline Zero = { row=0.; column=0. }
  static member inline (+) (a: Point, b: Point) =
    { row=a.row + b.row; column=a.column + b.column }

type Path =
  { head : Point
    tail : Point }

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