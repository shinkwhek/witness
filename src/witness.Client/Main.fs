module witness.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open Svg
open Pazzle
open PazzleRender
open Rules

open Microsoft.AspNetCore.Components
open Microsoft.JSInterop

/// ==== ==== Interop ==== ====
module Cmd =
  let inline ofAsyncSimple (onError: exn -> 'Msg) (task: Async<'Msg>) : Cmd<'Msg> =
    Cmd.ofAsync (fun () -> task) () id onError

type IJSRuntime with
  member inline this.invokeAsync<'t> (identifier, [<ParamArray>] args: obj[]) =
    if typeof<'t> = typeof<unit> then
      async {
        do! this.InvokeVoidAsync(identifier, args).AsTask() |> Async.AwaitTask
        return Unchecked.defaultof<_>
      }
    else
      this.InvokeAsync<'t>(identifier, args).AsTask() |> Async.AwaitTask

type IDeps = 
  abstract member JSRuntime: IJSRuntime with get

/// ==== ==== Expand ==== ====

let inline styles (styles: list<string>) : Attr =
  "style" => String.concat " " styles

/// ==== ==== model ==== ====

[<Struct>]
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
  member inline x.ToGridPoint step =
    { row= x.y / step; column= x.x / step }

type Movement =
  | NoMove
  | Vertical of float
  | Horizontal of float

type Positions =
  { movementp : Movement }

type Snake =
  { current : Point 
    pathes : Path list }

type LightPathes =
  { entrypoint : Point option 
    goalpath : Path option
    snake : Snake }

type Model =
  { grid : Grid
    mode : Mode
    solved : Solved
    positions : Positions
    lightpathes : LightPathes
    elements : Elements
    judgedElements : JudgedElement list }

let init (deps: IDeps) =
  { grid =
      { gridWidth = 4
        gridHeight = 4 }
    mode = Nothing
    solved = Default
    positions = { movementp = NoMove }
    lightpathes = { entrypoint = None
                    goalpath = None
                    snake = { current = Point.Zero; pathes = [] } }
    elements = [ Entry {row=0.0; column=0.0}
                 Entry {row=3.0; column=3.0}
                 Goal ( {row=0.0; column=3.0}, {row=0.; column=3.2} )
                 HexagonDot {row=1.; column=1.}
                 Square ( {row=0.5; column=1.5}, Black )
                 Triangle ( {row=0.5; column=2.5}, Orenge, Three)
                 Star ( {row=1.5; column=0.5}, White )
                 Star ( {row=1.5; column=1.5}, White )
                 Star ( {row=1.5; column=2.5}, White )
                 Square ( {row=2.5; column=0.5}, White )
                 Square ( {row=2.5; column=1.5}, Black )
                 Triangle ( {row=2.5; column=2.5}, Orenge, Two )
                 //Triangle ( {row=0.5; column=0.5}, Orenge, Two )
                 //Triangle ( {row=1.5; column=1.5}, Orenge, Two )
                 //Triangle ( {row=2.5; column=2.5}, Orenge, Three )
                 //Cancellation {row=1.5; column=2.5} 
                 ]
    judgedElements = [] }
  , Cmd.none

/// ==== ==== message ==== ====

[<Struct>]
type GridWH =
  | GridWidth
  | GridHeight

[<CompilationRepresentation(CompilationRepresentationFlags.UseNullAsTrueValue)>]
type Message =
  | Nop
  | Inclease of GridWH
  | Declease of GridWH
  | Mode of Mode
  | MouseMovement of dx:float * dy:float

/// ==== ==== update ==== ====

let updateElementPosition f wh model =
  let elements = model.elements
  let endX, endY = float <| model.grid.gridWidth - 1, float <| model.grid.gridHeight - 1
  let inline g x =
    match x, wh with
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
    | e, _ -> e
  let inline h x = 
    match x with
    | Entry _ | Goal _ -> true
    | _ -> false

  let elements =
    elements 
    |> List.filter h 
    |> List.map g
  { model with elements = elements }

let updateSnake next model = 
  let current = model.lightpathes.snake.current
  let pathes = model.lightpathes.snake.pathes
  
  match model.mode with
  | PathDraw entry ->
    match pathes with
    // add path
    | [] when let {column=x; row=y} = current - entry
              (abs y >= 1. || abs x >= 1.) ->
      let snake = { model.lightpathes.snake with pathes = [ {head=entry; tail=next} ] }
      let lightpathes = { model.lightpathes with snake = snake }
      { model with lightpathes = lightpathes }
    | {tail=pastTail}::_ 
      when let {column=x; row=y} = current - pastTail
           (abs y >= 1. || abs x >= 1.) ->
        let snake = { model.lightpathes.snake with pathes = {head=pastTail; tail=next}::pathes }
        let lightpathes = { model.lightpathes with snake = snake }
        { model with lightpathes = lightpathes }
    // del path
    | {head=pastHead; tail=pastTail}::pathes
      when let {column=backX; row=backY} = pastHead - pastTail
           let {column=x; row=y} = current - pastTail
           (max (backX*x) (backY*y) > 0.) ->
        let snake = { model.lightpathes.snake with pathes = pathes }
        let lightpathes = { model.lightpathes with snake = snake }
        { model with lightpathes = lightpathes }

    | _ ->
      model
  | _ -> model

let updateLightPath model =
  let movestep = 200.
  let grid = model.grid
  let width, height = float <| grid.gridWidth-1, float <| grid.gridHeight-1
  let current = model.lightpathes.snake.current
  let pathes = model.lightpathes.snake.pathes

  match model.mode with
    | PathDraw entry ->
      let pastPath = match pathes with
                     | [] -> { head=entry; tail=entry }
                     | path::_ -> { head=path.head; tail=path.tail }
      match model.positions.movementp with
      | NoMove -> model
      | Horizontal d ->
        let next = if d > 0.
                   then  { pastPath.tail with column = pastPath.tail.column+1. }
                   else { pastPath.tail with column = pastPath.tail.column-1. }
        let inline guard d =
          if ( width >= current.column && current.column >= 0. 
               && (0.4 > abs(current.row - pastPath.tail.row)) )
             || isOnElement current model.elements
          then {row=pastPath.tail.row; column = current.column + d/movestep}
          else current

        let snake = { model.lightpathes.snake with current = guard d }
        let lightpathes = { model.lightpathes with snake = snake }
        { model with lightpathes = lightpathes }
        |> updateSnake next

      | Vertical d ->
        let next = if d > 0. then { pastPath.tail with row = pastPath.tail.row+1. }
                             else { pastPath.tail with row = pastPath.tail.row-1. }
        let inline guard d =
          if ( height >= current.row
               && current.row >= 0.
               && (0.4 > abs(current.column - pastPath.tail.column)) )
             || isOnElement current model.elements
          then {row=current.row + d/movestep; column=pastPath.tail.column}
          else current

        let snake = { model.lightpathes.snake with current = guard d }
        let lightpathes = { model.lightpathes with snake = snake }
        { model with lightpathes = lightpathes }
        |> updateSnake next

    | _ -> model

let update (deps: IDeps) message model =
  let grid = model.grid
  match message with
  /// ---- mouse action ----
  | MouseMovement (dx, dy) ->
    let inline guard x y =
      match x,y with
      | x,y when x=0. && y =0. ->
        NoMove
      | x,y when abs x > abs y ->
        Horizontal <| x
      | _ ->
        Vertical <| y

    let positions = { model.positions with movementp = guard dx dy }
    { model with positions = positions }
    |> updateLightPath
    , Cmd.none
  // --- Nop ---
  | Nop -> model, Cmd.none
  // --- edit grid ---
  | Inclease GridWidth ->
    let model = updateElementPosition (fun x -> x + 1.0) GridWidth model
    let grid = { grid with gridWidth = grid.gridWidth + 1 }
    { model with grid = grid }, Cmd.none
  | Inclease GridHeight ->
    let model = updateElementPosition (fun x -> x + 1.0) GridHeight model
    let grid = { grid with gridHeight = grid.gridHeight + 1 }
    { model with grid = grid }, Cmd.none
  | Declease GridWidth ->
    let model = updateElementPosition (fun x -> x - 1.0) GridWidth model
    let grid = { grid with gridWidth = grid.gridWidth - 1 }
    { model with grid = grid }, Cmd.none
  | Declease GridHeight ->
    let model = updateElementPosition (fun x -> x - 1.0) GridHeight model
    let grid = { grid with gridHeight = grid.gridHeight - 1 }
    { model with grid = grid }, Cmd.none
  /// --- mode switch ---
  | Mode (PathDraw entry) ->
    let snake = { current = entry; pathes = [] }
    let lightpathes = { entrypoint=Some entry; goalpath=None; snake = snake }
    { model with mode = PathDraw entry
                 solved = Default
                 lightpathes = lightpathes }, Cmd.none
  | Mode Nothing ->
    let { lightpathes = lightpathes }, _ = init deps
    { model with mode = Nothing
                 lightpathes = lightpathes
                 solved = Default
                 judgedElements = []}, Cmd.none
  /// ---- Judgement ---
  | Mode Judgement ->
    let {current=current; pathes=pathes} = model.lightpathes.snake
    let elements = model.elements
    if isOnGoal current elements
    then
      let judgedElements = runJudge model.elements pathes model.grid
      let solved = if judgedElements |> List.forall (fun {satisfy=s} -> s)
                   then Solved 
                   else Miss
      let lightpathes =
        match solved, pathes with
        | Solved, {head=_; tail=tail}::_ ->
          let goalpath = { head=tail; tail=current }
          { model.lightpathes with goalpath=Some goalpath }
        | _ ->
          let i, _ = init deps
          i.lightpathes
      { model with mode = Nothing
                   solved = solved
                   lightpathes = lightpathes
                   judgedElements = judgedElements }, Cmd.none
    else
      let { lightpathes=lightpathes }, _ = init deps
      { model with mode = Nothing
                   lightpathes = lightpathes }, Cmd.none
    
/// ==== ==== view ==== ====

let isOnGoal model =
  match model.mode with
  | PathDraw _ ->
    let {current=current; pathes=pathes} = model.lightpathes.snake
    if isOnGoal current model.elements
    then Mode Judgement
    else Mode Nothing
  | _ -> Mode Nothing

let renderEntryTouch dispatch model grid =
  let inline f elm =
    match elm with
    | Entry p -> group [ attr.classes ["PuzzleEntry"]
                         "fill-opacity" => 0. ]
                       [ renderEntry [ on.click (fun _ -> dispatch <| Mode (PathDraw p)) ]
                                     p
                                     grid ]
    | _ -> Node.Empty
  List.map f model.elements

let renderElements dispatch model grid =
  let inline f elm =
    match elm with
    | Entry p ->
      group [ //on.click (fun _ -> dispatch (Mode (PathDraw p)))
              attr.classes ["PuzzleEntry"]
            ]
            [ renderEntry [ "stroke" => color2str Black
                            "fill" => color2str Black
                            on.click (fun _ -> dispatch <| Mode (PathDraw p))
                            //attr.classes ["PuzzleEntry"]
                          ] p grid
            ]
    | Goal (p,t) ->
      group [ "stroke" => color2str Black; "fill" => color2str Black ] [
        renderGoal (p,t) grid
      ]
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
    | Cancellation p ->
      let color = color2str White
      renderCancellation [ "stroke" => color
                           "fill" => color ] p grid
  model.elements |> List.map f

let redboxElements model grid =
  let attr = [ "fill" => "#ff6347" 
               "fill-opacity" => 0.5 ]
  let inline f {elm=elm; satisfy=satisfy} =
    match elm, satisfy with
    | HexagonDot p, false ->
      renderRed attr p grid (grid.Step*0.2)
    | Square (p,_), false
    | Star (p,_), false
    | Triangle (p,_,_), false
    | Cancellation p, false ->
      renderRed attr p grid (grid.Step - grid.Step*0.2)
    | _ -> Empty
  model.judgedElements |> List.map f

let inline DisplayWhenMiss solved =
  match solved with
  | Miss -> "display" => "inline"
  | _ -> "display" => "none"

let renderLightPath dispatch lightpathes grid =
  match lightpathes with
  | {entrypoint=Some entry; snake = { current=current; pathes=[] } } ->
    (group [ ]
           [ renderEntry [ ] entry grid ])
    ::[ renderLine entry current grid ]
  | {entrypoint=Some entry; snake = { current=current; pathes=pathes } } ->
    let p2 = pathes.Head.tail
    let path : Path = { head=current; tail=p2}
    (group [ ]
           [ renderEntry [ ] entry grid ])
    ::(path::pathes |> List.map (fun {head=p1; tail=p2} -> renderLine p1 p2 grid))
  | _ -> []

let inline solvedGlow solved =
  match solved with
  | Solved -> "filter:url(#glow);"
  | _ -> ""

let view (deps: IDeps) model dispatch =
  let grid = model.grid
  let offset = string (float grid.Step / 2.)
  let defs = defs []
                  [ filter [ "id" => "glow" ] 
                           [ elt "feGaussianBlur" [ "stdDeviation" => 2.5
                                                    "result" => "coloredBlur" ] []
                             elt "feMerge" [] [
                               elt "feMergeNode" [ "in" => "coloredBlur" ] []
                               elt "feMergeNode" [ "in" => "SourceGraphic" ] []
                             ] ] ]
  div []
      [
      div [ attr.classes [ "container1" ] ]
          [ div [] [ text <| "gridWidth: " + string grid.gridWidth
                     button [ on.click (fun _ -> dispatch (Inclease GridWidth)) ] [ text "+" ]
                     button [ on.click (fun _ -> dispatch (Declease GridWidth)) ] [ text "-" ] ]
            div [] [ text <| "gridHeight: " + string grid.gridHeight
                     button [ on.click (fun _ -> dispatch (Inclease GridHeight)) ] [ text "+" ]
                     button [ on.click (fun _ -> dispatch (Declease GridHeight)) ] [ text "-" ] ]
            //div [] [ text <| "Mode: " + string model.mode ]
            //div [] [ text <| "movement: " + string model.positions.movementp ]
            //div [] [ text <| "Solved?: " + string model.solved ]
            div [ attr.classes [ "puzzle" ] ]
                [ svg [ "width" => grid.Width
                        "height" => grid.Height
                        "xmlns" => "http://www.w3.org/2000/svg"
                        "version" => "1.1" ]
                      <| List.append
                        [defs]
                        [ group [ "transform" => "translate(" 
                                                 + offset + ","
                                                 + offset + ")" ]
                                [ group [ "stroke" => color2str Black
                                          "fill" => color2str Black ]
                                        (grid |> renderGrid) 
                                  group [ DisplayWhenMiss model.solved ]
                                        (grid |> redboxElements model)
                                  group [ attr.id "PuzzleElements" ] (grid |> renderElements dispatch model) 
                                  group [ "stroke" => color2str White
                                          "fill" => color2str White  
                                          styles [ solvedGlow model.solved ] ]
                                        (grid |> renderLightPath dispatch model.lightpathes) ]
                        ] ]
          ]
      script [ "type" => "text/javascript" ] [
        text """
'use strict';
var entryElements = document.getElementById("PuzzleElements").getElementsByClassName("PuzzleEntry")
                    || document.createElement('input');

document.exitPointerLock = document.exitPointerLock ||
                           document.mozExitPointerLock;
function endS() {
  document.exitPointerLock();
  snakeEnd();
}

function setPointerLock(a) {
  a.requestPointerLock = a.requestPointerLock ||
                         a.mozRequestPointerLock;
  a.onclick = function() {
    a.requestPointerLock();
  };
}
Array.prototype.forEach.call(entryElements, setPointerLock);

function lockChangeAlert() {
  let cond = function(a) {
    return (document.pointerLockElement === a ||
            document.mozPointerLockElement === a);
  };
  if (Array.prototype.some.call(entryElements, cond) ) {
    //console.log('The pointer lock status is now locked.');
    document.addEventListener("click", endS, {once: true});
    //document.addEventListener("mousemove", updatePosition, false);
    document.onmousemove = updatePosition;
  } else {
    //console.log("The pointer lock status is now unlocked.");
    //document.removeEventListener("click", endS);
    //document.removeEventListener("mousemove", updatePosition, false);
    document.onmousemove = doNothing();
  }
}

if ("onpointerlockchange" in document) {
  document.onpointerlockchange = lockChangeAlert;
} else if ("onmozpointerlockchange" in document) {
  document.onmozpointerlockchange = lockChangeAlert;
}

function doNothing() {}

function updatePosition(e) {
  DotNet.invokeMethodAsync('witness.Client', 'Main.MyApp.UpdatePositionAsync', e.movementX, e.movementY);
}
function snakeEnd() {
  DotNet.invokeMethodAsync('witness.Client', 'Main.MyApp.SnakeEndAsync');
}
"""
    ] 
  ]

type MyApp() =
  inherit ProgramComponent<Model, Message>()
  
  static let msgDispatched = new Event<Message>()
  static member val MsgDispatched = msgDispatched.Publish

  [<Inject>]
  member val JSRuntime = Unchecked.defaultof<IJSRuntime> with get,set

  [<JSInvokable("Main.MyApp.UpdatePositionAsync")>]
  static member UpdatePositionAsync (dx: float, dy: float) =
    msgDispatched.Trigger (MouseMovement (dx, dy))
    Threading.Tasks.Task.CompletedTask

  [<JSInvokable("Main.MyApp.SnakeEndAsync")>]
  static member SnakeEndAsync () =
    msgDispatched.Trigger (Mode Judgement)
    Threading.Tasks.Task.CompletedTask

  override this.Program = 
    let deps =
      { new IDeps with
          member __.JSRuntime with get() = this.JSRuntime }
    Program.mkProgram (fun _ -> init deps) (update deps) (view deps)
    |> Program.withSubscription (fun _ -> Cmd.ofSub (fun dispatch -> MyApp.MsgDispatched.Add dispatch))
