module witness.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Bolero.Html.attr
open Svg
open Pazzle
open PazzleRender
open Rules

/// ==== ==== Expand ==== ====

let inline styles (styles: list<string>) : Attr =
  "style" => String.concat " " styles

/// ==== ==== model ==== ====

type Mode =
  | Nothing
  | Judgement
  | PathDraw of entrypoint: Point

type Position =
  { x: float
    y: float }
  static member inline Zero = {x=0.; y=0.}
  static member inline (-) (a: Position, b: Position) =
    { x = a.x - b.x; y = a.y - b.y }
  member inline p.SwitchDirection =
    match p.x, p.y with
    | x,y when abs x > abs y->
       { x = x; y = 0. }
    | _,y ->
       { x = 0.; y = y }
  member inline x.ToGridPoint step =
    { row= x.y / step; column= x.x / step }

type Positions =
  { currentp : Position
    basep : Position
    history : Position list }
  member inline x.Diff =
    x.currentp - x.basep     

type LightPathes =
  { entrypoint : Point option 
    goalpath : Path option
    current : Point
    pathes : Path list }

type Model =
  { grid : Grid
    mode : Mode
    solved : Solved
    positions : Positions
    lightpathes : LightPathes
    elements : Elements
    judgedElements : JudgedElement list }

let initModel =
  { grid =
      { gridWidth = 4
        gridHeight = 4 }
    mode = Nothing
    solved = Default
    positions = { currentp = Position.Zero
                  basep = Position.Zero
                  history = [] } 
    lightpathes = { entrypoint = None
                    goalpath = None
                    current = Point.Zero
                    pathes = [] }
    elements = [ Entry {row=0.0; column=0.0}
                 Goal ( {row=0.0; column=3.0}, {row=0.; column=3.2} )
                 Triangle ( {row=0.5; column=0.5}, Orenge, Two )
                 Triangle ( {row=1.5; column=1.5}, Orenge, Two )
                 Triangle ( {row=2.5; column=2.5}, Orenge, Three ) ]
    judgedElements = [] }

/// ==== ==== message ==== ====

type GridWH =
  | GridWidth
  | GridHeight

type Message =
  | Inclease of GridWH
  | Declease of GridWH
  | Mode of Mode
  | GetPosition of Position

/// ==== ==== update ==== ====

let updateElementPosition f wh model =
  let elements = model.elements
  let endX, endY = float <| model.grid.gridWidth - 1, float <| model.grid.gridHeight - 1
  let g = (fun x -> match x, wh with
                      // ---- Entry ----
                      | Entry p, GridWidth when p.column = endX ->
                        Entry {row=p.row; column=f p.column} 
                      | Entry p, GridHeight when p.row = endY ->
                        Entry {row=f p.row; column=p.column}
                      // ---- Goal ----
                      | Goal (p,t), GridWidth when p.column = endX ->
                        Goal ( {row=p.row; column=f p.column},
                               {row=t.row; column=f t.column} )
                      | Goal (p,t), GridHeight when p.row = endY ->
                        Goal ( {row=f p.row; column=p.column},
                               {row=f t.row; column=t.column} )
                      | e, _ -> e )
  let inline h x = 
    match x with
      | Entry _ | Goal _ -> true
      | _ -> false
  let elements = List.filter h elements
  let elements = List.map g elements
  { model with elements = elements }

let inline updateBasePosition model =
  let positions = model.positions
  let history = positions.basep::positions.history
  let positions = { positions with basep = positions.currentp; history=history }
  { model with positions = positions }

let inline updateBaseBackToHistory model =
  let positions = model.positions
  match positions.history with
    | [] -> model
    | h::l ->
      let positions = { positions with basep = h; history = l }
      { model with positions = positions }

let updateSnake t pastPath nextPoint model =
  let grid = model.grid
  let maxX, maxY = float <| grid.gridWidth, float <| grid.gridHeight
  let {pathes=pathes} = model.lightpathes
  let {row=backY; column=backX} = pastPath.head - pastPath.tail
  let isInsidegrid =
    let x,y = nextPoint.column, nextPoint.row
    not (x = maxX || y = maxY || x = -1. || y = -1.)
  match t, pathes with
    | {row=y; column=x}, pathes
      when isInsidegrid && ( abs x >= 1. || abs y >= 1. ) ->
        let newPath = {head=pastPath.tail; tail=nextPoint}
        let lightpathes = { model.lightpathes with current=nextPoint
                                                   pathes=newPath::pathes }
        { model with lightpathes = lightpathes }
        |> updateBasePosition
    | {row=y; column=x}, _::pathes
      when backX * x > 0. && (0.3 > abs y) ->
        let lightpathes = { model.lightpathes with pathes=pathes }
        { model with lightpathes = lightpathes }
        |> updateBaseBackToHistory
    | {row=y; column=x}, _::pathes
      when backY * y > 0. && 0.3 > abs x ->
        let lightpathes = { model.lightpathes with pathes=pathes }
        { model with lightpathes = lightpathes }
        |> updateBaseBackToHistory
    | _ -> model
  
let updateLightPath model =
  let grid = model.grid
  match model.mode with
    | PathDraw entry ->
      let {current=current; pathes=pathes} = model.lightpathes
      let inline ignorePointOutsideGrid p =
        let width, height = float <| grid.gridWidth-1, float <| grid.gridHeight-1
        match p with
        | {row=y; column=x}
          when (width >= x && x >= 0. && height >= y && y >= 0.) ||
               isOnElement p model.elements -> p
        | _ -> current
      let pastHead, pastTail = match pathes with
                                 | [] -> entry, entry
                                 | path::_ -> path.head, path.tail
      let t = model.positions.Diff.ToGridPoint grid.Step
      let directionUnit = t.SwitchDirection.Unit
      let nextPoint = pastTail + directionUnit
      let current = match directionUnit with
                      | {row=0.; column=_} ->
                        let t = abs t.column
                        (1. - t)*pastTail + t*nextPoint
                        |> ignorePointOutsideGrid
                      | {row=_; column=0.} ->
                        let t = abs t.row
                        (1. - t)*pastTail + t*nextPoint
                        |> ignorePointOutsideGrid
                      | _ -> current
      let lightpathes = { model.lightpathes with current=current
                                                 entrypoint=Some entry
                                                 pathes=pathes }
      { model with lightpathes = lightpathes }
      |> updateSnake t {head=pastHead; tail=pastTail} nextPoint
    | _ -> model


let update message model =
  let grid = model.grid
  match message with
  /// --- edit grid ---
  | Inclease GridWidth ->
    let model = updateElementPosition (fun x -> x + 1.0) GridWidth model
    let grid = { grid with gridWidth = grid.gridWidth + 1 }
    { model with grid = grid }
  | Inclease GridHeight ->
    let model = updateElementPosition (fun x -> x + 1.0) GridHeight model
    let grid = { grid with gridHeight = grid.gridHeight + 1 }
    { model with grid = grid }
  | Declease GridWidth ->
    let model = updateElementPosition (fun x -> x - 1.0) GridWidth model
    let grid = { grid with gridWidth = grid.gridWidth - 1 }
    { model with grid = grid }
  | Declease GridHeight ->
    let model = updateElementPosition (fun x -> x - 1.0) GridHeight model
    let grid = { grid with gridHeight = grid.gridHeight - 1 }
    { model with grid = grid }
  /// --- mode switch ---
  | Mode (PathDraw entry) ->
    let model = updateBasePosition model
    let lightpathes = { model.lightpathes with entrypoint=Some entry }
    { model with mode = PathDraw entry
                 solved = Default
                 lightpathes = lightpathes }
  | Mode Nothing ->
    initModel
  /// ---- mouse action ----
  | GetPosition p ->
    let positions = { model.positions with currentp = p }
    let model = { model with positions = positions }
    model
    |> updateLightPath
  /// ---- Judgement ---
  | Mode Judgement ->
    let positions = { model.positions with basep = Position.Zero
                                           history = [] }
    let {current=current; pathes=pathes} = model.lightpathes
    let judgedElements = runJudge model.elements pathes model.grid
    let solved = if List.forall (fun {elm=_; satisfy=satisfy} -> satisfy )
                                judgedElements
                         then Solved else Miss
    let lightpathes =
      match solved, pathes with
        | Solved, {head=_; tail=tail}::_ ->
          let goalpath = { head=tail; tail=current }
          { model.lightpathes with goalpath=Some goalpath }
        | _ -> initModel.lightpathes
    { model with mode = Nothing
                 solved = solved
                 lightpathes = lightpathes
                 positions = positions
                 judgedElements = judgedElements }
    
/// ==== ==== view ==== ====

let isOnGoal model =
  match model.mode with
    | PathDraw _ ->
      let {current=current; pathes=pathes} = model.lightpathes
      if isOnGoal current model.elements
      then
        printfn "Judgement!"
        Mode Judgement
      else
        (Mode Nothing)
    | _ -> (Mode Nothing)

let renderElements dispatch model grid =
  let inline f elm =
    match elm with
      | Entry p ->
          renderEntry [ "stroke" => color2str Black
                        "fill" => color2str Black
                        classes [ "PuzzleEntry" ]
                        "tabindex" => "1"
                        on.focus (fun _ -> dispatch (Mode (PathDraw p)))
                        on.blur (fun _ -> dispatch (isOnGoal model)) ]
                      p grid
      | Goal (p,t) ->
        renderGoal [ "stroke" => color2str Black
                     "fill" => color2str Black ] (p,t) grid
      | HexagonDot p ->
        renderHexagonDot [ "stroke" => "gray"
                           "fill" => "gray" ] p grid
      | Square (p, color) ->
        let color = color2str color
        renderSquare [ "stroke" => color
                       "fill" => color ] p grid
      | Star (p, color) ->
        let color = color2str color
        renderStar [ "stroke" => color
                     "fill" => color ] p grid
      | Triangle (p, color, count) ->
        let color = color2str color
        renderTriangle [ "stroke" => color
                         "fill" => color ] p count grid
  (List.map f model.elements)

let setEntryClicky dispatch model grid =
  let inline f elm =
    match elm with
      | Entry p ->
        renderEntry [ classes [ "PuzzleEntry" ]
                      "fill-opacity" => 0.
                      ]
                    p grid
      | _ -> Empty
  (List.map f model.elements)

let redboxElements model grid =
    let attr = [ "fill" => "#ff6347" 
                 "fill-opacity" => 0.5 ]
    let inline f {elm=elm; satisfy=satisfy} =
      match elm, satisfy with
        | HexagonDot p, false ->
          renderRed attr p grid (grid.Step*0.2)
        | Square (p,_), false
        | Star (p,_), false
        | Triangle (p,_,_), false ->
          renderRed attr p grid (grid.Step - grid.Step*0.2)
        | _ -> Empty
    List.map f model.judgedElements

let inline DisplayWhenMiss solved =
  match solved with
    | Miss -> "display" => "inline"
    | _ -> "display" => "none"

let renderLightPath lightpathes grid =
  match lightpathes with
    | {entrypoint=Some entry; current=current; pathes=[]} ->
      List.append
        [ renderEntry [] entry grid ]
        [ renderLine [] entry current grid ]
    | {entrypoint=Some entry; current=current; pathes=pathes} ->
      let p2 = pathes.Head.tail
      let path : Path = { head=current; tail=p2}
      List.append
        [ renderEntry [] entry grid ]
        (List.map (fun {head=p1; tail=p2} -> renderLine [] p1 p2 grid) <| path::pathes)
    | _ -> []

let inline mouseHide model =
  match model.mode with
    | PathDraw _ -> "cursor: none;"
    | _ -> ""

let inline solvedGlow solved =
  match solved with
    | Solved -> "filter:url(#glow);"
    | _ -> ""

let view model dispatch =
  let grid = model.grid
  let defs = defs []
                  [ filter [ "id" => "glow" ] 
                           [ elt "feGaussianBlur" [ "stdDeviation" => 2.5
                                                    "result" => "coloredBlur" ] []
                             elt "feMerge" [] [
                               elt "feMergeNode" [ "in" => "coloredBlur" ] []
                               elt "feMergeNode" [ "in" => "SourceGraphic" ] []
                             ] ] ]
  div [ classes [ "container1" ]
        on.mousemove (fun e -> dispatch (GetPosition {x=e.ClientX; y=e.ClientY}))
        styles [ mouseHide model ] ]
      [ div [] [ text <| "gridWidth: " + string grid.gridWidth
                 button [ on.click (fun _ -> dispatch (Inclease GridWidth)) ] [ text "+" ]
                 button [ on.click (fun _ -> dispatch (Declease GridWidth)) ] [ text "-" ] ]
        div [] [ text <| "gridHeight: " + string grid.gridHeight
                 button [ on.click (fun _ -> dispatch (Inclease GridHeight)) ] [ text "+" ]
                 button [ on.click (fun _ -> dispatch (Declease GridHeight)) ] [ text "-" ] ]
        div [] [ text <| "Mode: " + string model.mode ]
        div [] [ text <| "Solved?: " + string model.solved ]
        div [ classes [ "puzzle" ] ]
            [ svg [ "width" => grid.Width
                    "height" => grid.Height
                    "xmlns" => "http://www.w3.org/2000/svg"
                    "version" => "1.1" ]
                  <| List.append
                    [defs]
                    [ group [ "transform" => "translate(" 
                                             + string (float grid.Step / 2.) + ","
                                             + string (float grid.Step / 2.) + ")" ]
                            [ group [ "stroke" => color2str Black
                                      "fill" => color2str Black ]
                                    (grid |> renderGrid) 
                              group [ DisplayWhenMiss model.solved ]
                                    (grid |> redboxElements model)
                              group [] (grid |> renderElements dispatch model) 
                              group [ "stroke" => color2str White
                                      "fill" => color2str White  
                                      styles [ solvedGlow model.solved ] ]
                                    (grid |> renderLightPath model.lightpathes) ]
                      //group [ "transform" => "translate(" 
                      //                       + string (float grid.Step / 2.) + ","
                      //                       + string (float grid.Step / 2.) + ")" ]
                      //      [ group [] (grid |> setEntryClicky dispatch model) ] 
                      ] ]
        div [] [ text <| "lightpathes: " + string model.lightpathes ] ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override __.Program = Program.mkSimple (fun _ -> initModel) update view
