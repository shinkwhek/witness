module PazzleRender

open Bolero
open Bolero.Html
open Svg
open Pazzle

let localPosition p (grid: Grid) =
  let step = grid.Step
  let x = step * (p.column)
  let y = step * (p.row)
  x, y, step

let renderLine attr (p1: Point) (p2: Point) (grid: Grid) =
  let x1, y1, step = localPosition p1 grid
  let x2, y2, _ = localPosition p2 grid
  let attr =
    List.append [ "stroke-width" => step * 0.2
                  "stroke-linecap" => "round" ] attr
  line attr x1 y1 x2 y2

/// ---- ---- render grid ---- ----

let renderGrid grid =
  let endX = grid.gridWidth - 1
  let endY = grid.gridHeight - 1
  seq {
    for i in [0..endX] do
      let i = float i
      yield renderLine [] {row=0.0; column=i} {row=float endY; column=i} grid
    for j in [0..endY] do
      let j = float j
      yield renderLine [] {row=j; column=0.0} {row=j; column=float endX} grid
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

let renderGoal attr (p,t) grid =
  renderLine attr p t grid

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