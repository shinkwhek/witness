module PazzleParts

open Bolero
open Bolero.Html
open Svg
open Pazzle

let gridOffset grid =
  { grid with width = grid.width + 1
              height = grid.height + 1 }

let line (p1: Point) (p2: Point) (color: Color) (grid: Grid) =
  let color = color2str color
  let (stepX, stepY) = (grid.pixelWidth / float grid.width, 
                        grid.pixelHeight / float grid.height)
  let step = min stepX stepY
  elt "line" [ "x1" => step * (p1.column + 1.0)
               "y1" => step * (p1.row + 1.0)
               "x2" => step * (p2.column + 1.0) 
               "y2" => step * (p2.row + 1.0)
               "stroke" => color
               "stroke-width" => step / 4.6
               "stroke-linecap" => "round" ] []

let renderGrid grid =
  let startX, endX = 0, grid.width - 1
  let startY, endY = 0, grid.height - 1
  seq {
    yield rect [ "fill" => color2str BackGround
                 "rx" => 10
                 "ry" => 10 ] 0 0 grid.pixelWidth grid.pixelHeight
    for i in [startX..endX] do
      let i = float i
      yield line {row=0.0; column=i} {row=float endY; column=i} Black <| gridOffset grid
    for j in [startY..endY] do
      let j = float j
      yield line {row=j; column=0.0} {row=j; column=float endX} Black <| gridOffset grid
  } |> Seq.toList
