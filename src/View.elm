module View exposing (view)

import Model exposing (..)
import Html exposing (Html)
import Element exposing (Element, el, text, row, alignRight, fill, width, height, rgb255, spacing, centerY, padding, centerX)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import GameView


view : Model -> Html Msg
view model =
  Element.layout
    []
    (el
      [ width fill
      , height fill
      , Background.color (rgb255 40 40 40)
      ]
      (Element.html (GameView.view model.strands)))


myRowOfStuff : Element msg
myRowOfStuff =
    row [ width fill, centerY, spacing 30 ]
        [ myElement
        , myElement
        , el [ alignRight ] myElement
        ]


myElement : Element msg
myElement =
    el
        [ Background.color (rgb255 240 0 245)
        , Font.color (rgb255 255 255 255)
        , Border.rounded 3
        , padding 30
        ]
        (text "stylish!")

