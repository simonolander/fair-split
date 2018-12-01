module View exposing (view)

import Model exposing (..)
import Html exposing (Html)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import GameView
import List.Extra


view : Model -> Html Msg
view model =
  let 
    gameView = 
      viewGame model.strands

    footer =
      viewFooter model.strands
  in
    Element.layout
      []
      (column
        [ width fill
        , height fill
        , Background.color (rgb255 40 40 40)
        ]
        [ gameView
        , footer 
        ]
      )


viewFooter strands = 
  let 
    (yourGemCount, theirGemCount) = 
      countGems strands

    completed = 
      yourGemCount == theirGemCount

    viewGemCount : (Gem, Int) -> Element msg
    viewGemCount (gem, count) =
      row
        [ spacing 5
        ]
        [ el 
            [ width (px 20) 
            , height (px 20)
            , Background.color 
                ( case gem of
                    Blue -> rgb255 0 0 255
                    Yellow -> rgb255 255 255 0
                )
            , Border.rounded 10
            , Border.color (rgb255 100 100 100)
            , Border.width 2
            ] 
            none
        , text (String.fromInt count)
        ]

    viewGemsCount gemsCount borderColor =
      gemsCount
      |> List.map viewGemCount
      |> row 
          [ spacing 20
          , Border.rounded 4
          , padding 4
          , Border.color borderColor
          , Border.width 3
          ]

    yourGemCountView = 
      viewGemsCount yourGemCount (rgb255 0 128 0)

    theirGemCountView = 
      viewGemsCount theirGemCount (rgb255 255 0 0)

    resetButtonView = 
      Input.button
        [ padding 4
        , Border.rounded 4
        , Border.color (rgb 0 0 0)
        , Border.width 1
        , Background.color (rgb255 255 255 255)
        ]
        { label = text "Reset"
        , onPress = Maybe.Just Reset
        }

    continueButtonView = 
      Input.button
        [ padding 4
        , Border.rounded 4
        , Border.color (rgb255 0 180 0)
        , Border.width 2
        , Background.color (rgb255 200 255 200)
        ]
        { label = text "Next"
        , onPress = Maybe.Just NextClicked
        }

   
  in
    el 
      [ alignBottom
      , padding 12
      , Background.color (rgb255 80 80 80)
      , width fill 
      ] 
      ( row
          [ width fill
          ]
          [ row
              [ height fill
              , spacing 30
              ]
              [ yourGemCountView
              , theirGemCountView
              , resetButtonView
              , if completed then continueButtonView else none
              ]
          ]
      )


viewGame : Strands -> Element Msg
viewGame = Element.html << GameView.view

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


countGems : Strands -> (List (Gem, Int), List (Gem, Int))
countGems strands = 
  let 
    distinctGems = 
      List.concat strands
      |> List.Extra.uniqueBy getGemOrdinal

    yourGems =
      strands
      |> List.indexedMap 
          (\ index strand -> if modBy 2 index == 0 then strand else [])
      |> List.concat
    
    theirGems = 
      strands
      |> List.indexedMap 
          (\ index strand -> if modBy 2 index == 1 then strand else [])
      |> List.concat

    getGemCount : List Gem -> List (Gem, Int)
    getGemCount gems = 
      distinctGems
      |> List.map 
          (\ gem -> (gem, List.Extra.count ((==) gem) gems))

    yourGemCount = 
      getGemCount yourGems

    theirGemCount = 
      getGemCount theirGems
  in
    (yourGemCount, theirGemCount)


