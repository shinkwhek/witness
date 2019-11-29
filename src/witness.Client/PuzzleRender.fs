module PuzzleRender

open System

open Bolero
open Bolero.Html
open Svg
open Puzzle

let localPosition p (grid: Grid) =
  let step = grid.Step
  let x = step * (p.column)
  let y = step * (p.row)
  x, y, step

let renderLine (p1: Point) (p2: Point) (grid: Grid) =
  let x1, y1, step = localPosition p1 grid
  let x2, y2, _ = localPosition p2 grid
  let attr = [ "stroke-width" => step * 0.2
               "stroke-linecap" => "round" ]
  line attr x1 y1 x2 y2

/// ---- ---- render grid ---- ----

let renderGrid grid =
  let endX = grid.gridWidth - 1
  let endY = grid.gridHeight - 1
  seq {
    for i in [0..endX] do
      let i = float i
      yield renderLine {row=0.0; column=i} {row=float endY; column=i} grid
    for j in [0..endY] do
      let j = float j
      yield renderLine {row=j; column=0.0} {row=j; column=float endX} grid
  } |> Seq.toList

/// ---- ---- render elements ---- ----

let renderRed attr p grid step =
  let x, y, _ = localPosition p grid
  let x, y = x - step/2., y - step/2.
  rect attr x y step step

let renderEntry attr p grid =
  let x, y, step = localPosition p grid
  let r = step * 0.25
  circle attr x y r

let renderGoal (p,t) grid =
  renderLine p t grid

let renderHexagonDot attr p grid =
  let x, y, step = localPosition p grid
  let linewidth = step * 0.2
  let points = [ (-0.5, 0.); (-0.25, 0.43); (0.25, 0.43);
                 (0.5, 0.); (0.25, -0.43); (-0.25, -0.43); ]
  let inline f (a,b) = (a*linewidth*0.9 + x, b*linewidth*0.9 + y)
  let points = List.map f points
  polygon attr points

let renderSquare attr p grid =
  let x, y, step = localPosition p grid
  let width = step * 1.2 / 3.
  let x, y = x - width/2., y - width/2.
  let attr = [ "rx" => string 10
               "ry" => string 10 ] @ attr
  rect attr x y width width

let renderStar attr p grid =
  let x, y, step = localPosition p grid
  let rotate = [ "transform" => "rotate(45, " + string x + ", " + string y + ")" ]
  let width = step / 3.
  let x, y = x - width/2., y - width/2.
  group attr
        [ rect [] x y width width
          rect rotate x y width width ]

let renderTriangle attr p count grid =
  let x, y, step = localPosition p grid
  let width = step / 6.
  let height = sin(Math.PI / 3.) * width
  let trianglePoints = [ (0., -height/2. ); (width/2., height/2.); (-width/2., height/2.) ]
  let inline f(a,b) = (a+x, b+y)
  let inline offset o (a,b) = (a+o, b)
  group attr
        (match count with
          | One ->
            let trianglePoints1 = trianglePoints |> List.map f
            [ polygon attr trianglePoints1 ]
          | Two ->
            let trianglePoints1 = trianglePoints |> List.map (f >> offset (- width*1.3/2.)) 
            let trianglePoints2 = trianglePoints |> List.map (f >> offset (width*1.3/2.))
            [ polygon attr trianglePoints1
              polygon attr trianglePoints2 ]
          | Three ->
            let trianglePoints1 = trianglePoints |> List.map f
            let trianglePoints2 = trianglePoints |> List.map (f >> offset (-width*1.3))
            let trianglePoints3 = trianglePoints |> List.map (f >> offset (width*1.3))
            [ polygon attr trianglePoints1
              polygon attr trianglePoints2
              polygon attr trianglePoints3 ] )

let renderCancellation attr p grid =
  let x, y, step = localPosition p grid
  let inline rotate r = [ "transform" => "rotate(" + string r + ", "
                                         + string x + ", "
                                         + string y + ")" ]
  let height = step / 6.
  let width = height / 2.
  let x = x - width/2.
  group attr 
        [ rect (rotate 60) x y width height
          rect (rotate 180) x y width height
          rect (rotate -60) x y width height ]
