module Svg

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

let lineClose = "Z"

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
