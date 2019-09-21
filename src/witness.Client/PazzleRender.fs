module PazzleRender

open Bolero
open Bolero.Html
open Svg
open Pazzle

let gridOffset grid =
  { grid with gridWidth = grid.gridWidth + 1
              gridHeight = grid.gridHeight + 1 }

let inline localPosition p grid =
  let (stepX, stepY) = (grid.pixelWidth / float grid.gridWidth, 
                        grid.pixelHeight / float grid.gridHeight)
  let step = min stepX stepY
  let x = step * (p.column + 1.0)
  let y = step * (p.row + 1.0)
  x, y, step

let renderLine attr (p1: Point) (p2: Point) (color: Color) (grid: Grid) =
  let color = color2str color
  let x1, y1, step = localPosition p1 grid
  let x2, y2, _ = localPosition p2 grid
  let attr =
    List.append [ "stroke" => color
                  "stroke-width" => step * 0.2
                  "stroke-linecap" => "round" ] attr
  line attr x1 y1 x2 y2

/// ---- ---- render grid ---- ----

let renderGrid grid =
  let startX, endX = 0, grid.gridWidth - 1
  let startY, endY = 0, grid.gridHeight - 1
  seq {
    yield rect [ "fill" => color2str BackGround
                 "rx" => 10
                 "ry" => 10 ] 0 0 grid.pixelWidth grid.pixelHeight
    for i in [startX..endX] do
      let i = float i
      yield renderLine [] {row=0.0; column=i} {row=float endY; column=i} Black <| gridOffset grid
    for j in [startY..endY] do
      let j = float j
      yield renderLine [] {row=j; column=0.0} {row=j; column=float endX} Black <| gridOffset grid
  } |> Seq.toList

/// ---- ---- render elements ---- ----

let inline renderEntry attr p grid =
  let x, y, step = localPosition p grid
  let r = step * 0.25
  circle attr x y r

let inline renderGoal p grid =
  renderLine [] p { p with column = p.column + 0.3 } Black grid