module witness.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Svg
open Pazzle
open PazzleParts

type Model =
  { x : int
    grid : Grid
    pathes : Path list
    elements : Element list }

let initModel =
  { x = 0
    grid =
      { pixelWidth = 500.
        pixelHeight = 500.
        width = 5
        height = 5 }
    pathes = []
    elements = [] }

type IncDec =
  | Width
  | Height

type Message =
  | Inclease of IncDec
  | Declease of IncDec

let update message model =
  match message with
  | Inclease Width -> { model with grid = { model.grid with width = model.grid.width + 1 } }
  | Inclease Height -> { model with grid = { model.grid with height = model.grid.height + 1 } }
  | Declease Width -> { model with grid = { model.grid with width = model.grid.width - 1 } }
  | Declease Height -> { model with grid = { model.grid with height = model.grid.height - 1 } }

let view model dispatch =
  let grid = model.grid
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
             (grid |> renderGrid)
             ] ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override __.Program = Program.mkSimple (fun _ -> initModel) update view
