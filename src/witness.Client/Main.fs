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
  | PathDraw of entrypoint: Point

type Model =
  { grid : Grid
    mode : Mode
    pathes : Path list
    elements : Element list }

let initModel =
  { grid =
      { pixelWidth = 500.
        pixelHeight = 500.
        width = 5
        height = 5 }
    mode = Nothing
    pathes = []
    elements = [ Entry {row=0.0; column=0.0}
                 Goal {row=0.0; column=4.0} ] }

/// ---- ---- message ---- ----

type IncDec =
  | Width
  | Height

type Message =
  | Inclease of IncDec
  | Declease of IncDec
  | Mode of Mode

let update message model =
  match message with
  | Inclease Width -> { model with grid = { model.grid with width = model.grid.width + 1 } }
  | Inclease Height -> { model with grid = { model.grid with height = model.grid.height + 1 } }
  | Declease Width -> { model with grid = { model.grid with width = model.grid.width - 1 } }
  | Declease Height -> { model with grid = { model.grid with height = model.grid.height - 1 } }
  | Mode m -> { model with mode = m }

/// ---- ---- view ---- ----

let renderElements dispatch elements grid =
  seq {
    for element in elements do
      match element with
      | Entry p ->
        yield renderEntry [ on.click (fun _ -> dispatch (Mode (PathDraw p))) ] p <| gridOffset grid
      | Goal p ->
        yield renderGoal p <| gridOffset grid
  } |> Seq.toList

let renderUserPath dispatch mode grid =
  match mode with
  | Nothing -> []
  | PathDraw p ->
    [ renderEntry [ "fill" => color2str White ] p <| gridOffset grid ]

let view model dispatch =
  let mode = model.mode
  let grid = model.grid
  let elements = model.elements
  let render = (grid |> renderGrid)
                |? (grid |> renderElements dispatch elements)
                |? (grid |> renderUserPath dispatch mode)
  div []
    [ div [] [ text <| "pixelWidth: " + string grid.pixelWidth ]
      div [] [ text <| "pixelHeight: " + string grid.pixelHeight ]
      div [] [ p [] [ text <| "width: " + string grid.width ]
               button [ on.click (fun _ -> dispatch (Inclease Width)) ] [ text "+" ]
               button [ on.click (fun _ -> dispatch (Declease Width)) ] [ text "-" ] ]
      div [] [ p [] [text <| "height: " + string grid.height ]
               button [ on.click (fun _ -> dispatch (Inclease Height)) ] [ text "+" ]
               button [ on.click (fun _ -> dispatch (Declease Height)) ] [ text "-" ] ]
      div [] 
        [svg [ "width" => grid.pixelWidth
               "height" => grid.pixelHeight
               "version" => "1.1" ]
             render ] ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override __.Program = Program.mkSimple (fun _ -> initModel) update view
