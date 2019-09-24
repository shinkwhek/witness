module witness.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Svg
open Pazzle
open PazzleRender

let inline (|?) x y = List.append x y

/// ==== ==== model ==== ====

type Mode =
  | Nothing
  | PathDraw of entrypoint: Point * goal: Point option

type Position =
  { x: float
    y: float }
  static member inline Zero = {x=0.; y=0.}
  static member inline (-) (a: Position, b: Position) =
    { x = a.x - b.x; y = a.y - b.y }
  member p.SwitchDirection =
    if abs p.x > abs p.y
    then { x = p.x; y = 0. }
    else { x = 0.; y = p.y }
  member x.ToGridPoint step =
    { row= x.y / step; column= x.x / step }

type Positions =
  { currentp : Position
    basep : Position }
  member inline x.Diff =
    x.currentp - x.basep

type Model =
  { grid : Grid
    mode : Mode
    positions : Positions
    pathes : Point * Path list
    elements : Element list }

let initModel =
  { grid =
      { gridWidth = 4
        gridHeight = 4 }
    mode = Nothing
    positions = { currentp = Position.Zero
                  basep = Position.Zero } 
    //pathes = Point.Zero, [ {head= Point.Zero
    //                        tail= {row=0.; column=1.} } ]
    pathes = Point.Zero, []
    elements = [ Entry {row=0.0; column=0.0}
                 Goal ( {row=0.0; column=3.0}, {row=0.; column=3.2} ) ] }

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

let inline updateElementPosition f wh model =
  let elements = model.elements
  let endX, endY = model.grid.gridWidth - 1, model.grid.gridHeight - 1
  let f = (fun x -> match x, wh with
                      | Entry p, GridWidth when p.column = float endX ->
                        Entry {row=p.row; column=f p.column} 
                      | Entry p, GridHeight when p.row = float endY ->
                        Entry {row=f p.row; column=p.column}
                      | Goal (p,t), GridWidth when p.column = float endX ->
                        Goal ( {row=p.row; column=f p.column},
                               {row=t.row; column=f t.column} )
                      | Goal (p,t), GridHeight when p.row = float endY ->
                        Goal ( {row=f p.row; column=p.column},
                               {row=f t.row; column=t.column} )
                      | e, _ -> e )
  { model with elements = List.map f elements }

let inline updateLightPath model =
  let positions = model.positions
  let grid = model.grid
  let inline checkInGrid p pathes model =
    match p with
      | {row=y; column=x}
        when float (grid.gridWidth - 1) >= x && x >= 0.
          && float (grid.gridHeight - 1) >= y && y >= 0. ->
        { model with pathes = p, pathes }
      | {row=y; column=x}
        when List.forall (function | Goal (_,t) ->
                                     (t.row = y && t.column >= x)
                                     || (t.column = x && t.row >= y)
                                   | _ -> true)
                         model.elements ->
        { model with pathes = p, pathes }
      | _ -> model

  match model.mode with
    | PathDraw(entry, goal) ->
      match model.pathes with
        | _, [] -> 
          let newpath = positions.Diff.SwitchDirection.ToGridPoint grid.Step
          let newpath = entry + newpath
          model |> checkInGrid newpath []
        | _, path::otherpath ->
          let newpath = positions.Diff.SwitchDirection.ToGridPoint grid.Step
          let newpath = path.tail + newpath
          model |> checkInGrid newpath (path::otherpath)
    | _ -> model

let inline updateBasePosition model =
  let positions = model.positions
  let positions = { model.positions with basep = positions.currentp }
  { model with positions = positions }

let inline updateLightPathSnake model =
  let positions = model.positions
  let grid = model.grid
  match model.mode with
    | PathDraw(entry, goal) ->
      let inline getHeadPath s =
        match s with
          | [] -> entry, entry
          | s -> s.Head.head, s.Head.tail
      let current, pathes = model.pathes
      let ohh, oht = getHeadPath pathes
      match positions.Diff.ToGridPoint grid.Step with
        | {column=x} when abs x >= 0.95 ->
          let x = Math.Round (x, 0) + oht.column
          let current = { current with column= x }
          let pathes = current, { head=oht; tail=current }::pathes
          let model = updateBasePosition model
          { model with pathes = pathes }
        | {row=y} when abs y >= 0.95 ->
          let y = Math.Round (y, 0) + oht.row
          let current = { current with row= y }
          let pathes = current, { head=oht; tail=current }::pathes
          let model = updateBasePosition model
          { model with pathes = pathes }          
        | _ -> model
    | _ -> model


let update message model =
  match message with
  /// --- edit grid ---
  | Inclease GridWidth ->
    let model = updateElementPosition (fun x -> x + 1.0) GridWidth model
    let grid = { model.grid with gridWidth = model.grid.gridWidth + 1 }
    { model with grid = grid }
  | Inclease GridHeight ->
    let model = updateElementPosition (fun x -> x + 1.0) GridHeight model
    let grid = { model.grid with gridHeight = model.grid.gridHeight + 1 }
    { model with grid = grid }
  | Declease GridWidth ->
    let model = updateElementPosition (fun x -> x - 1.0) GridWidth model
    let grid = { model.grid with gridWidth = model.grid.gridWidth - 1 }
    { model with grid = grid }
  | Declease GridHeight ->
    let model = updateElementPosition (fun x -> x - 1.0) GridHeight model
    let grid = { model.grid with gridHeight = model.grid.gridHeight - 1 }
    { model with grid = grid }
  /// --- mode switch ---
  | Mode (PathDraw(entry, goal)) ->
    let model = updateBasePosition model
    { model with mode = PathDraw(entry, goal) }
  | Mode Nothing ->
    { model with mode = Nothing; pathes = Point.Zero, [] }
  /// ---- mouse action ----
  | GetPosition p ->
    let model = { model with positions = { model.positions with currentp = p } }
    model
    |> updateLightPath
    |> updateLightPathSnake
    
 
/// ==== ==== view ==== ====

let inline renderElements dispatch model grid =
  seq {
    for element in model.elements do
      match element with
       | Entry p ->
          yield renderEntry [ "class" => "PuzzleEntry"
                              "tabindex" => "0"
                              "stroke" => color2str Black
                              "fill" => color2str Black 
                              on.focus
                                (fun _ -> dispatch (Mode (PathDraw (p, None))))
                              on.blur (fun _ -> dispatch (Mode Nothing)) ]
                            p  grid
        | Goal (p,t) ->
          yield renderGoal [ "stroke" => color2str Black
                             "fill" => color2str Black ] (p,t) grid
  } |> Seq.toList

let inline renderLightPath dispatch model grid =
  match model.mode with
  | Nothing -> []
  | PathDraw (entry, goal) ->
    let pathes =
      match model.pathes with
        | current, [] ->
          [ renderLine [] entry current grid ]
        | current, pathes ->
          let p2 = pathes.Head.tail
          let path : Path = { head=current; tail=p2}
          List.map (fun {head=p1; tail=p2} -> renderLine [] p1 p2 grid) <| path::pathes
    pathes
    |? [ renderEntry [ ]
                     entry grid ]

let inline mouseHide model =
  match model.mode with
    | PathDraw _ -> "cursor: none;"
    | _ -> ""

let view model dispatch =
  let grid = model.grid
  let positions = model.positions
  let render =  [ group [ "transform" => "translate(" 
                                         + string (float grid.Step / 2.) + ","
                                         + string (float grid.Step / 2.) + ")" ]
                        [ group [ "stroke" => color2str Black
                                  "fill" => color2str Black ]
                                (grid |> renderGrid) 
                          group [] (grid |> renderElements dispatch model) 
                          group [ "stroke" => color2str White
                                  "fill" => color2str White ]
                                (grid |> renderLightPath dispatch model) ] ]
  div [ "class" => "container1"
        on.mousemove (fun e -> dispatch (GetPosition {x=e.ClientX; y=e.ClientY})) ]
    [ div [] [ text <| "gridWidth: " + string grid.gridWidth
               button [ on.click (fun _ -> dispatch (Inclease GridWidth)) ] [ text "+" ]
               button [ on.click (fun _ -> dispatch (Declease GridWidth)) ] [ text "-" ] ]
      div [] [ text <| "gridHeight: " + string grid.gridHeight
               button [ on.click (fun _ -> dispatch (Inclease GridHeight)) ] [ text "+" ]
               button [ on.click (fun _ -> dispatch (Declease GridHeight)) ] [ text "-" ] ]
      div [] [ text <| "current pos: " + string positions.currentp ]
      div [] [ text <| "base pos: " + string positions.basep ]
      div [] [ text <| "diff point: " + string ( positions.Diff.ToGridPoint grid.Step ) ]
      div [ "class" => "puzzle"
            "style" => mouseHide model ]
        [svg [ "width" => grid.Width
               "height" => grid.Height
               "version" => "1.1" ]
             render ]
      div [] [ text <| "pathes: " + string model.pathes ] ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override __.Program = Program.mkSimple (fun _ -> initModel) update view
