module Pazzle

open System

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
  member inline x.Norm = sqrt (x.row**2. + x.column**2.)
  member inline x.Unit =
    let norm = x.Norm
    { row = Math.Round(x.row / norm,0)
      column = Math.Round(x.column / norm,0) }
  static member inline (+) (a: Point, b: Point) =
    { row=a.row + b.row; column=a.column + b.column }
  static member inline (-) (a: Point, b:Point) =
    { row=a.row - b.row; column=a.column - b.column }
  static member inline (*) (a: float, b:Point) =
    { row= a*b.row ; column = a*b.column }
  member inline x.SwitchDirection =
    if abs x.row > abs x.column 
    then { x with column = 0. }
    else { x with row = 0. }

type Path =
  { head : Point
    tail : Point }    

type Element =
  | Entry of pos: Point
  | Goal of head: Point * tail: Point
  | HexagonDot of pos: Point

type Elements = Element list

let inline isOnElement p elements =
  let inline f element =
    match element with
      | Goal (_,tail) ->
        (tail.row = p.row && tail.column >= p.column) ||
        (tail.column = p.row && tail.row >= p.column)
      | _ -> true
  (List.forall f elements)

let inline isOnGoal p elements =
  let inline f element =
    match element with
      | Goal (_, tail) ->
        0.1 > abs (tail.row - p.row) && 0.1 > abs (tail.column - p.column)
      | _ -> false
  (List.exists f elements)

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