module Rules

open FSharpPlus

open Pazzle

type JudgedElement =
  { elm : Element
    satisfy : bool }

[<Struct>]
type JudgedElements =
  JE of JudgedElement list
  with
    static member inline Return elms =
      JE <| (List.map (fun e -> {elm=e; satisfy=false} ) elms)
    static member inline (>>=) (JE a, f) =
      f a

type Solved = Default | Solved | Miss

let inline skipEntryGoal jes =
  let inline f {elm=elm; satisfy=satisfy} =
    match elm with
      | Entry _ | Goal _ ->
        { elm=elm; satisfy=true }
      | _ ->
        { elm=elm; satisfy=satisfy }
  List.map f jes |> JE

let inline judgeHexagonDot pathes jes =
  let inline rule {row=y; column=x} path =
    let { head = {row=hy; column=hx}
          tail = {row=ty; column=tx} } = path
    let dx1, dy1 = tx-hx, ty-hy
    let dx2, dy2 = x-hx, y-hy
    let dx3, dy3 = x-tx, y-ty
    dx1*dy2 - dy1*dx2 = 0.
    && ( 1. >= dx2**2. + dy2**2. )
    && ( 1. >= dx3**2. + dy3**2. )

  let inline f {elm=elm; satisfy=satisfy} =
    match elm, satisfy with
      | HexagonDot p, false when List.exists (rule p) pathes ->
        { elm=elm; satisfy=true }
      | _ ->
        { elm=elm; satisfy=satisfy }
  List.map f jes |> JE

let inline judgeRules elements pathes =
  JudgedElements.Return elements
  >>= skipEntryGoal
  >>= judgeHexagonDot pathes

let inline runJudge elements pathes =
  let (JE jes) = judgeRules elements pathes
  jes