module Update exposing (update)

import Model exposing (..)
import Generators exposing (..)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
      Reset -> 
        ( { model
          | strands = resetStrands model.strands
          }
        , Cmd.none
        )
      Cut index -> 
        ( { model
          | strands = cutStrands index model.strands
          }
        , Cmd.none
        )
      NextClicked ->
        ( model
        , newStrandsMsg
        )
      NewStrands strands -> 
        ( { model
          | strands = strands
          }
        , Cmd.none
        )


resetStrands : Strands -> Strands 
resetStrands strands = 
  [ List.concat strands ]


cutStrands : Int -> Strands -> Strands
cutStrands index strands =
  if index < 0
  then 
    strands
  else 
    case strands of
      (head :: tail) -> 
        if index < List.length head - 1
        then 
          List.take (index + 1) head :: List.drop (index + 1) head :: tail
        else
          head :: cutStrands (index - List.length head) tail
      [] -> []
