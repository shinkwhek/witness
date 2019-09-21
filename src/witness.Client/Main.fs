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
  | PathDraw of entrypoint: Point * 
                goal: Point option

type Model =
  { grid : Grid
    mode : Mode
    currentPositon : Point
    pathes : Path list
    elements : Element list }

let initModel =
  { grid =
      { pixelWidth = 500.
        pixelHeight = 500.
        gridWidth = 5
        gridHeight = 5 }
    mode = Nothing
    currentPositon = { row=0.0; column=0.0 }
    pathes = [ ({row=0.0; column=0.0}, {row=0.0; column=1.0})
               ({row=0.0; column=1.0}, {row=1.0; column=1.0})
               ({row=1.0; column=1.0}, {row=1.0; column=2.0}) ]
    elements = [ Entry {row=0.0; column=0.0}
                 Goal {row=0.0; column=4.0} ] }

/// ---- ---- message ---- ----

type GridWH =
  | GridWidth
  | GridHeight

type Message =
  | Inclease of GridWH
  | Declease of GridWH
  | Mode of Mode
  | Position of Point

let updateElementPosition model f wh =
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

let update message model =
  match message with
  | Inclease GridWidth ->
    let model = updateElementPosition model (fun x -> x + 1.0) GridWidth
    { model with grid = { model.grid with gridWidth = model.grid.gridWidth + 1 } }
  | Inclease GridHeight ->
    let model = updateElementPosition model (fun x -> x + 1.0) GridHeight
    { model with grid = { model.grid with gridHeight = model.grid.gridHeight + 1 } }
  | Declease GridWidth ->
    let model = updateElementPosition model (fun x -> x - 1.0) GridWidth
    { model with grid = { model.grid with gridWidth = model.grid.gridWidth - 1 } }
  | Declease GridHeight ->
    let model = updateElementPosition model (fun x -> x - 1.0) GridHeight
    { model with grid = { model.grid with gridHeight = model.grid.gridHeight - 1 } }
  | Mode m -> { model with mode = m }
  | Position p -> { model with currentPositon = p }

/// ---- ---- view ---- ----

let renderElements dispatch model grid =
  seq {
    for element in model.elements do
      match element with
      | Entry p ->
        yield renderEntry [ on.click
                              (fun _ -> dispatch (Mode (PathDraw (p, None))))]
                          p <| gridOffset grid
      | Goal p ->
        yield renderGoal p <| gridOffset grid
  } |> Seq.toList

let renderLightPath dispatch model grid =
  match model.mode with
  | Nothing -> []
  | PathDraw (entry, goal) ->
    let pathes =
      List.map (fun (p1, p2) -> renderLine [] p1 p2 White <| gridOffset grid) model.pathes
    pathes
    |? [ renderEntry [ "fill" => color2str White
                       on.click (fun _ ->
                                  match goal with
                                    | _ -> dispatch (Mode Nothing)) ]
                     entry <| gridOffset grid ]


let view model dispatch =
  let grid = model.grid
  let render = (grid |> renderGrid)
                |? (grid |> renderElements dispatch model)
                |? (grid |> renderLightPath dispatch model)
  div [ ]
    [ div [] [ text <| "pixelWidth: " + string grid.pixelWidth ]
      div [] [ text <| "pixelHeight: " + string grid.pixelHeight ]
      div [] [ text <| "gridWidth: " + string grid.gridWidth
               button [ on.click (fun _ -> dispatch (Inclease GridWidth)) ] [ text "+" ]
               button [ on.click (fun _ -> dispatch (Declease GridWidth)) ] [ text "-" ] ]
      div [] [ text <| "gridHeight: " + string grid.gridHeight
               button [ on.click (fun _ -> dispatch (Inclease GridHeight)) ] [ text "+" ]
               button [ on.click (fun _ -> dispatch (Declease GridHeight)) ] [ text "-" ] ]
      div [] [ text <| "current postion: " + string model.currentPositon ]
      div [] 
        [svg [ "width" => grid.pixelWidth
               "height" => grid.pixelHeight
               "version" => "1.1" 
               on.mousemove (fun e -> dispatch (Position { row=e.ClientY; column=e.ClientX })) ]
             render ] ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override __.Program = Program.mkSimple (fun _ -> initModel) update view
