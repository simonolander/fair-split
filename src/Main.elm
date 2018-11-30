module Main exposing (..)

import Browser
import Model exposing (..)
import Update exposing (update)
import View exposing (view)


init : ( Model, Cmd Msg )
init =
    let 
        strands = [ [Blue, Yellow, Yellow, Blue, Blue, Blue, Yellow, Yellow, Blue, Blue, Yellow, Yellow, Blue, Blue, Yellow, Yellow, Blue, Blue, Yellow, Yellow, Blue, Blue, Yellow, Yellow, Blue, Blue, Yellow, Yellow, Blue, Blue, Yellow, Yellow, Blue, Blue] ]
        
        model = 
            { strands = strands
            }
    in
        ( model, Cmd.none )


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
