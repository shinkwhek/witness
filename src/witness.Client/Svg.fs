module Svg

open FSharpPlus

open Bolero
open Bolero.Html

type PathAttribute =
  | Global
  | Relative

let inline moveTo x y t =
  let t =
    match t with
    | Global -> "M "
    | Relative -> "m "
  t + string x + " " + string y + " "

let inline lineTo x y t =
  let t =
    match t with
    | Global -> "L "
    | Relative -> "l "
  t + string x + " " + string y + " "

let inline lineHorizontalTo x t =
  let t =
    match t with
    | Global -> "H "
    | Relative -> "h "
  t + string x + " "

let inline lineVertivalTo y t =
  let t =
    match t with
    | Global -> "V "
    | Relative -> "v "
  t + string y + " "

let [<Literal>] LineClose = "Z"

let bezier2 (a1, a2) x y t =
  let t =
    match t with
    | Global -> "Q "
    | Relative -> "q "
  let a1, a2, x, y = string a1, string a2, string x, string y
  t + a1 + " " + a2 + ", " + x + " " + y + " "

let bezier2Link x y t =
  let t =
    match t with
    | Global -> "T "
    | Relative -> "t "
  let x, y = string x, string y
  t + x + ", " + y + " "

let bezier3 (a1, a2) (b1, b2) x y t =
  let t =
    match t with
    | Global -> "C "
    | Relative -> "c "
  let a1, a2, b1, b2, x, y = string a1, string a2, string b1, string b2, string x, string y
  t + a1 + " " + a2 + ", " + b1 + " " + b2 + ", " + x + " " + y + " "

let bezier3Link (a1, a2) x y t =
  let t =
    match t with
    | Global -> "S "
    | Relative -> "s "
  let a1, a2, x, y = string a1, string a2, string x, string y
  t + a1 + " " + a2 + ", " + x + " " + y + " "

let path attrs (l : string list) =
  let param = "d" => List.fold (+) "" l
  let atters = param :: attrs
  elt "path" atters []

let circle attrs cx cy r =
  let attrs =
    List.append [ "cx" => cx
                  "cy" => cy
                  "r" => r ] attrs
  elt "circle" attrs []

let rect attrs x y width height =
  let attrs = 
    List.append [ "x" => x 
                  "y" => y
                  "width" => width
                  "height" => height ] attrs
  elt "rect" attrs []

let line attrs x1 y1 x2 y2 =
  let attrs =
    List.append [ "x1" => x1
                  "y1" => y1
                  "x2" => x2
                  "y2" => y2 ] attrs
  elt "line" attrs []

let polygon attrs plst =
  let inline f (a: float, b: float) = string a + "," + string b
  let plst = List.map f plst
  let attrs =
    List.append [ "points" => String.concat " " plst ] attrs
  elt "polygon" attrs []

let inline group attr nodes =
  elt "g" attr nodes

let inline defs attr nodes =
  elt "defs" attr nodes

let inline filter attr nodes =
  elt "filter" attr nodes