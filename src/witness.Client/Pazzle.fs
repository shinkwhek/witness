module Pazzle

open System

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
  member inline x.LookUp _ =
    let x = x - {row=1.; column=0.}
    if x.row >= 0. then Some x else None
  member inline x.LookDown grid = 
    let x = x + {row=1.; column=0.}
    if float <| grid.gridHeight-1 >= x.row then Some x else None
  member inline x.LookRight grid =
    let x = x + {row=0.; column=1.}
    if float <| grid.gridWidth-1 >= x.column then Some x else None
  member inline x.LookLeft _ =
    let x = x - {row=0.; column=1.}
    if x.column >= 0. then Some x else None
  member x.LookAround grid =
    let up, down = x.LookUp grid, x.LookDown grid
    let right, left = x.LookRight grid, x.LookLeft grid
    let rec filter r lst =
      match lst with
        | [] -> r
        | None::l -> filter r l
        | (Some a)::l -> filter (a::r) l
    filter [] [up; down; right; left]

type Path =
  { head : Point
    tail : Point }
  with
    static member inline Dot {head=h1; tail=t1} {head=h2; tail=t2} =
      let v1, v2 = t1 - h1, t2 - h2
      v1.row * v2.row + v1.column * v2.column
    member x.Straddle p1 p2 =
      let s1, s2 = x.head - p1, x.tail - p2
      1. > s1.Norm && 1. > s2.Norm
      && Path.Dot x {head=p1; tail=p2} = 0.
     
type Element =
  | Entry of pos: Point
  | Goal of pos: Point * tail: Point
  | HexagonDot of pos: Point
  | Square of pos: Point * color: Color
  with
    member inline x.GetPos =
      match x with
        | Entry pos | Goal (pos,_) | HexagonDot pos | Square (pos,_) -> pos
        

type Elements = Element list

let inline isOnElement {row=y; column=x} elements =
  let inline f element =
    match element with
      | Goal (_, {row=ty; column=tx}) ->
        (ty = y && tx >= x && x >= 0.) ||
        (tx = x && ty >= y && y >= 0.)
      | _ -> true
  (List.forall f elements)

let isOnGoal p elements =
  let inline f element =
    match element with
      | Goal (_, tail) ->
        0.1 > abs (tail.row - p.row) && 0.1 > abs (tail.column - p.column)
      | _ -> false
  (List.exists f elements)

