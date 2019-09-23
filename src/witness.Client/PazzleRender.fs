module PazzleRender

open Bolero
open Bolero.Html
open Svg
open Pazzle


let inline localPosition p (grid: Grid) =
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

let inline renderGrid grid =
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

let renderEntry attr p grid =
  let x, y, step = localPosition p grid
  let r = step * 0.25
  circle attr x y r

let renderGoal attr (p,t) grid =
  renderLine attr p t grid