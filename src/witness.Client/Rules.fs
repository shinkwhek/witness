module Rules

open FSharpPlus
open FSharpPlus.Data

open Pazzle

type JudgedElement =
  { elm : Element
    satisfy : bool }

type JudgedElements =
  JE of JudgedElement list
  with
    static member inline Return elms =
      JE <| (List.map (fun e -> {elm=e; satisfy=false} ) elms)
    static member inline (>>=) (JE a, f) =
      f a

[<Struct>]
type Solved = Default | Solved | Miss

let inline skipEntryGoal jes =
  let inline f {elm=elm; satisfy=satisfy} =
    match elm with
      | Entry _ | Goal _ ->
        { elm=elm; satisfy=true }
      | _ ->
        { elm=elm; satisfy=satisfy }
  jes |> List.map f |> JE

let rec setSet grid pathes (s: Set<Point>) past (p: Point) =
  let around = p.LookAround grid
               |> List.filter // inside pathes
                    (fun x -> List.forall
                                (fun (path: Path) -> not <| path.Straddle p x)
                                pathes)
               |> Set.ofList
  let around = match past with Some p -> around.Remove p | _ -> around
  let around = around |> Set.filter (fun x -> not <| Set.contains x s)
  if around.IsEmpty
  then s
  else let arounds = Set.map (setSet grid pathes ((around.Add p)+s) (Some p)) around
       Set.fold (+) (Set.ofList []) arounds

// ==== ==== Rules ==== ====

let judgeHexagonDot pathes jes =
  let inline rule {row=y; column=x} path =
    let { head = {row=hy; column=hx}
          tail = {row=ty; column=tx} } = path
    let dx1, dy1 = tx-hx, ty-hy
    let dx2, dy2 = x-hx, y-hy
    let dx3, dy3 = x-tx, y-ty
    dx1*dy2-dy1*dx2 = 0.
    && ( 1. >= dx2**2.+dy2**2. )
    && ( 1. >= dx3**2.+dy3**2. )
  let inline f {elm=elm; satisfy=satisfy} =
    match elm, satisfy with
      | HexagonDot p, false when List.exists (rule p) pathes ->
        { elm=elm; satisfy=true }
      | _ ->
        { elm=elm; satisfy=satisfy }
  map f jes |> JE

let judgeSquare pathes grid jes =
  let elements = jes |> List.map (fun {elm=elm} -> elm)  
  let inline rule color origin =
    let otherColorSquarePoints =
      elements
      |> List.filter (function | Square(_,c) when c<>color -> true
                               | _ -> false)
      |> List.map (fun x -> x.GetPos)
    let set = setSet grid pathes (Set.ofList []) None origin
    List.forall (fun p -> not <| Set.contains p set) otherColorSquarePoints

  let inline f {elm=elm; satisfy=satisfy} =
    match elm, satisfy with
      | Square (origin, color), false when rule color origin ->
        { elm=elm; satisfy=true }
      | _ ->
        { elm=elm; satisfy=satisfy }
  map f jes |> JE

let judgeStar pathes grid jes =
  let elements = jes |> List.map (fun {elm=elm} -> elm)
  let inline rule color origin =
    let otherColorPoints =
      elements
      |> List.filter (function | Square(_,c) | Star(_,c) when c=color -> true
                               | _ -> false )
      |> List.map (fun x -> x.GetPos)
      |> Set.ofList
    let set = setSet grid pathes (Set.ofList []) None origin
    let set = Set.filter (fun p -> Set.contains p set) otherColorPoints
    set.Count = 2

  let inline f {elm=elm; satisfy=satisfy} =
    match elm, satisfy with
      | Star (origin, color), false when rule color origin ->
        { elm=elm; satisfy=true }
      | _ ->
        { elm=elm; satisfy=satisfy }
  map f jes |> JE

let judgeTriangle pathes jes =
  let inline rule count (origin: Point) =
    let around = origin.LookAroundIgnoreGrid
                 |> List.filter // straddles
                      (fun x -> List.exists
                                  (fun (path: Path) -> path.Straddle origin x)
                                  pathes)
                 |> Set.ofList
    let count = match count with One -> 1 | Two -> 2 | Three -> 3
    around.Count = count

  let inline f {elm=elm; satisfy=satisfy} =
    match elm, satisfy with
      | Triangle (origin, _, count), false when rule count origin ->
        { elm=elm; satisfy=true }
      | _ ->
        {elm=elm; satisfy=satisfy}
  map f jes |> JE

// ==== ==== ==== ====

let judgeRules elements pathes grid =
  JudgedElements.Return elements
  >>= skipEntryGoal
  >>= judgeHexagonDot pathes
  >>= judgeSquare pathes grid
  >>= judgeStar pathes grid
  >>= judgeTriangle pathes

let inline runJudge elements pathes grid =
  let (JE jes) = judgeRules elements pathes grid
  jes
