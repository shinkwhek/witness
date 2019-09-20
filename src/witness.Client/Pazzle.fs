module Pazzle

type Grid =
  {
    pixelWidth : float
    pixelHeight : float
    width : int
    height : int }
  
type Point =
  { row : float
    column : float }

type Path = Point * Point

type Element =
  | Entry of pos: Point
  | Goal of pos: Point

type Color =
  | Black
  | White
  | BackGround
  with
    member x.ToStr =
      match x with
      | Black -> "black"
      | White -> "White"
      | BackGround -> "#5d6c7e"

let inline color2str (x: ^X) =
  (^X: (member ToStr : string) x)