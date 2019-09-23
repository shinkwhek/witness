module witness.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Svg
open Pazzle
open PazzleRender

let inline (|?) x y = List.append x y

/// ---- ---- model ---- ----

type Mode =
  | Nothing
  | PathDraw of entrypoint: Point * goal: Point option

type Position =
  { x: float
    y: float }
  static member inline Zero = {x=0.; y=0.}
  static member inline (-) (a: Position, b: Position) =
    { x = a.x - b.x; y = a.y - b.y }
  member inline p.SwitchDirection =
    if abs p.x > abs p.y
    then { x = p.x; y = 0. }
    else { x = 0.; y = p.y }
  member inline x.ToGridPoint step =
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
                 Goal {row=0.0; column=3.0} ] }

/// ---- ---- message ---- ----

type GridWH =
  | GridWidth
  | GridHeight

type Message =
  | Inclease of GridWH
  | Declease of GridWH
  | Mode of Mode
  | GetPosition of Position

/// ---- ---- update ---- ----

let inline updateElementPosition f wh model =
  let elements = model.elements
  let endX, endY = model.grid.gridWidth - 1, model.grid.gridHeight - 1
  let f = (fun x -> match x, wh with
                      | Entry p, GridWidth when p.column = float endX ->
                        Entry {row=p.row; column=f p.column} 
                      | Entry p, GridHeight when p.row = float endY ->
                        Entry {row=f p.row; column=p.column}
                      | Goal p, GridWidth when p.column = float endX ->
                        Goal {row=p.row; column=f p.column}
                      | Goal p, GridHeight when p.row = float endY ->
                        Goal {row=f p.row; column=p.column}
                      | e, _ -> e )
  { model with elements = List.map f elements }

let inline updateLightPath model =
  let positions = model.positions
  match model.mode with
   | Nothing -> model
    | PathDraw(entry, goal) ->
      match model.pathes with
      | _, [] -> 
        let newpath = positions.Diff.SwitchDirection.ToGridPoint model.grid.Step
        let newpath = entry + newpath
        { model with pathes = newpath, [] }
      | _, path::otherpath ->
        let newpath = positions.Diff.SwitchDirection.ToGridPoint model.grid.Step
        let newpath = path.tail + newpath
        { model with pathes = newpath, path::otherpath }

let inline updateBasePosition model =
  { model with positions = { model.positions with basep = model.positions.currentp } }

let update message model =
  match message with
  /// --- edit grid ---
  | Inclease GridWidth ->
    let model = updateElementPosition (fun x -> x + 1.0) GridWidth model
    { model with grid = { model.grid with gridWidth = model.grid.gridWidth + 1 } }
  | Inclease GridHeight ->
    let model = updateElementPosition (fun x -> x + 1.0) GridHeight model
    { model with grid = { model.grid with gridHeight = model.grid.gridHeight + 1 } }
  | Declease GridWidth ->
    let model = updateElementPosition (fun x -> x - 1.0) GridWidth model
    { model with grid = { model.grid with gridWidth = model.grid.gridWidth - 1 } }
  | Declease GridHeight ->
    let model = updateElementPosition (fun x -> x - 1.0) GridHeight model
    { model with grid = { model.grid with gridHeight = model.grid.gridHeight - 1 } }
  /// --- mode ---
  | Mode (PathDraw(entry, goal)) ->
    let model = updateBasePosition model
    { model with mode = PathDraw(entry, goal) }
  | Mode Nothing ->
    { model with mode = Nothing }
  /// ---- mouse action ----
  | GetPosition p ->
    let model = { model with positions = { model.positions with currentp = p } }
    let model = updateLightPath model
    model
 
/// ---- ---- view ---- ----

let renderElements dispatch model grid =
  seq {
    for element in model.elements do
      match element with
       | Entry p ->
          yield renderEntry [ on.click
                                (fun _ -> dispatch (Mode (PathDraw (p, None))))
                              "stroke" => color2str Black
                              "fill" => color2str Black ]
                            p  grid
        | Goal p ->
          yield renderGoal [ "stroke" => color2str Black
                             "fill" => color2str Black ] p grid
  } |> Seq.toList

let renderLightPath dispatch model grid =
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
    |? [ renderEntry [ on.click (fun _ ->
                                  match goal with
                                    | _ -> dispatch (Mode Nothing)) ]
                     entry grid ]

let view model dispatch =
  let grid = model.grid
  let positions = model.positions
  let render =  [ group [ "transform" => "translate(" + string (float grid.Step / 2.) + ","
                                                      + string (float grid.Step / 2.) + ")" ]
                        [ group [ "stroke" => color2str Black
                                  "fill" => color2str Black ]
                                (grid |> renderGrid) 
                          group [] (grid |> renderElements dispatch model) 
                          group [ "stroke" => color2str White
                                  "fill" => color2str White ]
                                (grid |> renderLightPath dispatch model) ] ]
  div []
    [ div [] [ text <| "gridWidth: " + string grid.gridWidth
               button [ on.click (fun _ -> dispatch (Inclease GridWidth)) ] [ text "+" ]
               button [ on.click (fun _ -> dispatch (Declease GridWidth)) ] [ text "-" ] ]
      div [] [ text <| "gridHeight: " + string grid.gridHeight
               button [ on.click (fun _ -> dispatch (Inclease GridHeight)) ] [ text "+" ]
               button [ on.click (fun _ -> dispatch (Declease GridHeight)) ] [ text "-" ] ]
      div [] [ text <| "current pos: " + string positions.currentp ]
      div [] [ text <| "base pos: " + string positions.basep ]
      div [] [ text <| "diff pos: " + string positions.Diff ]
      div [] [ text <| "pathes: " + string model.pathes ]
      div [ "class" => "puzzle" ]
        [svg [ "class" => "puzzle-body"
               "width" => grid.Width
               "height" => grid.Height
               "version" => "1.1"
               on.mousemove (fun e -> dispatch (GetPosition {x=e.ClientX; y=e.ClientY})) ]
             render ] ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override __.Program = Program.mkSimple (fun _ -> initModel) update view
