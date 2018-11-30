module Model exposing (..)

type Gem =
  Blue
  | Yellow


type alias Strand =
  List Gem


type alias Strands = 
  List Strand


type alias Model = 
  { strands: Strands
  }

  
type Msg
    = Reset
    | Cut Int
