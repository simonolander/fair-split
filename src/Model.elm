module Model exposing (..)

type Gem =
  Blue
  | Yellow


getGemOrdinal : Gem -> Int
getGemOrdinal gem =
  case gem of
    Blue -> 0
    Yellow -> 1


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
    | NextClicked
    | NewStrands Strands
