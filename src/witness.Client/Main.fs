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
    match p.x, p.y with
    | x,y when abs x > abs y->
       { x = x; y = 0. }
    | _,y ->
       { x = 0.; y = y }
  member x.ToGridPoint step =
    { row= x.y / step; column= x.x / step }

type Positions =
  { currentp : Position
    basep : Position
    history : Position list }
  member inline x.Diff =
    x.currentp - x.basep
     

type Model =
  { grid : Grid
    mode : Mode
    positions : Positions
    pathes : Point * Path list // currentPoint * oldPathes
    elements : Elements }

let initModel =
  { grid =
      { gridWidth = 4
        gridHeight = 4 }
    mode = Nothing
    positions = { currentp = Position.Zero
                  basep = Position.Zero
                  history = [] } 
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

let inline updateBasePosition model =
  let positions = model.positions
  let history = [positions.basep] |? positions.history
  let positions = { positions with basep = positions.currentp; history=history }
  { model with positions = positions }

let inline updateBaseBackToHistory model =
  let positions = model.positions
  match positions.history with
    | [] -> model
    | h::l ->
      let positions = { positions with basep = h; history = l }
      { model with positions = positions }

let inline updateSnake t pastPath nextPoint model =
  let current, pathes = model.pathes
  let {row=backY; column=backX} = pastPath.head - pastPath.tail
  match t, pathes with
    | {row=y; column=x}, pathes when abs x >= 1. || abs y >= 1. ->
      let newPath = {head=pastPath.tail; tail=nextPoint}
      { model with pathes = nextPoint, newPath::pathes }
      |> updateBasePosition
    | {row=y; column=x}, _::pathes when backX * x > 0. &&
                                        0.04 > abs y ->
      { model with pathes = current, pathes }
      |> updateBaseBackToHistory
    | {row=y; column=x}, _::pathes when backY * y > 0. &&
                                        0.04 > abs x ->
      { model with pathes = current, pathes }
      |> updateBaseBackToHistory
    | _ -> model
  
let inline updateLightPath model =
  let grid = model.grid
  match model.mode with
    | PathDraw(entry, goal) ->
      let current, pathes = model.pathes
      let inline ignorePointOutsideGrid p =
        let width, height = float (grid.gridWidth-1), float (grid.gridHeight-1)
        match p with
        | {row=y; column=x} when width >= x && x >= 0. && height >= y && y >= 0. -> p
        | p when Pazzle.isOnElement p model.elements -> p
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
      { model with pathes = current, pathes }
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
  | Mode (PathDraw(entry, goal)) ->
    let model = updateBasePosition model
    { model with mode = PathDraw(entry, goal) }
  | Mode Nothing ->
    let positions = { model.positions with basep = Position.Zero
                                           history = [] }
    { model with mode = Nothing
                 pathes = Point.Zero, []
                 positions = positions }
  /// ---- mouse action ----
  | GetPosition p ->
    let positions = { model.positions with currentp = p }
    let model = { model with positions = positions }
    model
    |> updateLightPath
   
    
 
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
                              on.focus (fun _ -> dispatch (Mode (PathDraw (p, None))))
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
      div [] [ text <| "history: " + string positions.history ]
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
