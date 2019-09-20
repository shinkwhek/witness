module witness.Client.Main

open Elmish
open Bolero
open Bolero.Html
open Svg

type Grid =
  { width : int
    height : int }

type Point =
  { row : int
    column : int }

type Path = Point * Point

type Model =
  { x : int
    grid : Grid
    pathes : Path list }

let initModel =
  { x = 0
    grid =
      { width = 5
        height = 5 }
    pathes = [] }

type Message =
  | Inclise
  | Declise

let update message model =
  match message with
  | Inclise -> { model with x = model.x + 1 }
  | Declise -> { model with x = model.x - 1 }

let view model dispatch =
  div []
    [ svg [ "version" => "1.1" ]
          [ path [ "fill" => "blue" ]
                 [ moveTo 10 10 Global
                   lineHorizontalTo 90 Global
                   lineVertivalTo 90 Global 
                   lineHorizontalTo 10 Global
                   lineClose ]
            circle [ "fill" => "red" ] 10 10 2
            circle [ "fill" => "red" ] 90 90 2
            circle [ "fill" => "red" ] 90 10 2
            circle [ "fill" => "red" ] 10 90 2 
            path [ "stroke" => "black"; "fill" => "transparent" ]
                 [ moveTo 10 10 Global
                   bezier3 (20, 20) (40, 20) 50 10 Global
                   bezier3Link (150, 150) 180 80 Global
                   moveTo 10 80 Global
                   bezier2 (52.5, 10) 95 80 Global
                   bezier2Link 180 80 Global ] ] ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  override __.Program = Program.mkSimple (fun _ -> initModel) update view
