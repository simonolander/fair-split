module Main exposing (..)

import Browser
import Model exposing (..)
import Update exposing (update)
import View exposing (view)
import Generators exposing (newStrandsMsg)


init : ( Model, Cmd Msg )
init =
    let 
        strands = []
        
        model = 
            { strands = strands
            }

        cmds = 
            Cmd.batch
                [ newStrandsMsg
                ]
    in
        ( model, cmds )


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
