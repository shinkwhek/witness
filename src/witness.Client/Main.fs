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

type Model =
  { grid : Grid
    mode : Mode
    basePosition : Point option
    currentPosition : Point
    pathes : Point * Path list
    elements : Element list }

let initModel =
  { grid =
      { gridWidth = 4
        gridHeight = 4 }
    mode = Nothing
    basePosition = None
    currentPosition = { row=0.0; column=0.0 }
    pathes = {row=1.0; column=2.5},
             [ ({row=0.0; column=1.0}, {row=1.0; column=1.0})
               ({row=0.0; column=0.0}, {row=0.0; column=1.0}) ]
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
  | Position of Point

/// ---- ---- update ---- ----

let updateElementPosition f wh model =
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
  | Mode (PathDraw(p, po)) ->
    { model with basePosition = Some model.currentPosition; mode = PathDraw(p, po) }
  | Mode Nothing ->
    { model with basePosition = None; mode = Nothing }
  | Position p -> { model with currentPosition = p }

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
      | current, pathes ->
        let _, p2 = pathes.Head
        let path : Path = p2, current
        List.map (fun (p1, p2) -> renderLine [] p1 p2 grid) <| path::pathes
    pathes
    |? [ renderEntry [ on.click (fun _ ->
                                  match goal with
                                    | _ -> dispatch (Mode Nothing)) ]
                     entry grid ]

let view model dispatch =
  let grid = model.grid
  let render =  [ group [ "transform" => "translate(" + string (float grid.Step / 2.) + ","
                                                      + string (float grid.Step / 2.) + ")" ]
                        [ group [ "stroke" => color2str Black
                                  "fill" => color2str Black ] (grid |> renderGrid) 
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
      div [] [ text <| "current position: " + string model.currentPosition ]
      div [] [ text <| "base position: " + string model.basePosition ]
      div [ "class" => "puzzle" ]
        [svg [ "class" => "puzzle-body"
               "width" => grid.Width
               "height" => grid.Height
               "version" => "1.1" 
               on.mousemove (fun e -> dispatch (Position { row=e.ClientY ; column=e.ClientX })) ]
             render ] ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override __.Program = Program.mkSimple (fun _ -> initModel) update view
