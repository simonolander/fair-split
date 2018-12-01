module GameView exposing (view)

import Model exposing (..)
import Svg exposing (..)
import Html exposing (Html)
import Svg.Attributes exposing (..)
import Svg.Events exposing (..)
import Array
import List.Extra

minX : Float
minX = 0.0

minY : Float
minY = 0.0

maxX : Float
maxX = 100.0

maxY : Float
maxY = 100.0

viewBoxString : String
viewBoxString = 
  String.join
    " "
    [ String.fromFloat minX 
    , String.fromFloat minY 
    , String.fromFloat maxX 
    , String.fromFloat maxY 
    ]

viewBoxWidth : Float
viewBoxWidth = maxX - minX

viewBoxHeight : Float
viewBoxHeight = maxY - minY

viewBoxMarginX : Float
viewBoxMarginX = viewBoxWidth * 0.1

viewBoxMarginY : Float
viewBoxMarginY = viewBoxHeight * 0.1

circleRadius : Float
circleRadius = viewBoxWidth * 0.017

type alias Position =
  { x: Float
  , y: Float
  }

type alias Wire = 
  { p1: Position
  , p2: Position
  , index: Int
  , cut: Bool
  , yours: Bool
  }

view : Strands -> Html Msg
view strands = 
  let 
    gems = 
      List.concat strands

    numberOfGems = 
      strands
      |> List.map List.length
      |> List.sum

    positions = 
      gemPositions numberOfGems

    wires = 
      List.tail positions 
      |> Maybe.withDefault []
      |> List.take (numberOfGems - 1)
      |> List.Extra.zip positions
      |> List.indexedMap
        (\ index ((x1, y1), (x2, y2)) -> 
          { p1 = Position x1 y1
          , p2 = Position x2 y2
          , index = index 
          , cut = isCut index strands
          , yours = isYours True index strands
          }
        )

    gemViews = 
      List.Extra.zip gems positions
      |> List.map viewGem

    wireViews =
      List.map viewWire wires

    elements = 
      List.concat
        [ wireViews
        , gemViews
        ]
  in
    svg
      [ width "100%"
      , height "80vh"
      , viewBox viewBoxString
      , preserveAspectRatio "xMidYMid meet"
      ]
      elements


viewGem : (Gem, (Float, Float)) -> Svg Msg
viewGem (gem, (x, y)) = 
  circle
    [ cx (String.fromFloat x)
    , cy (String.fromFloat y)
    , r (String.fromFloat circleRadius)
    , fill 
        (case gem of
          Blue -> "blue"
          Yellow -> "yellow"
        )
    , stroke "gray"
    , strokeWidth (String.fromFloat (circleRadius * 0.2))
    ]
    []


viewWire : Wire -> Svg Msg
viewWire wire =
  let 
    yourColor = 
      "green"
    
    theirColor = 
      "red"

    (strokeColor1, strokeColor2) = 
      if wire.yours
      then 
        (yourColor, theirColor)
      else
        (theirColor, yourColor)

    cursor =
      if wire.cut
      then 
        "auto"
      else
        "pointer"
    
    viewLine p1 p2 strokeColor = 
      line
        [ x1 (String.fromFloat p1.x)
        , y1 (String.fromFloat p1.y)
        , x2 (String.fromFloat p2.x)
        , y2 (String.fromFloat p2.y)
        , stroke strokeColor
        , strokeWidth (String.fromFloat (circleRadius * 0.5))
        , onClick (Cut wire.index)
        , strokeLinecap "round"
        , Svg.Attributes.cursor cursor
        ]
        []
  in
    if wire.cut
    then
      g []
        [ viewLine 
            wire.p1
            { x = wire.p1.x + (wire.p2.x - wire.p1.x) * 1 / 3
            , y = wire.p1.y + (wire.p2.y - wire.p1.y) * 1 / 3
            }
            strokeColor1
        , viewLine 
            { x = wire.p1.x + (wire.p2.x - wire.p1.x) * 2 / 3
            , y = wire.p1.y + (wire.p2.y - wire.p1.y) * 2 / 3
            }
            wire.p2
            strokeColor2
        ]
    else
      viewLine wire.p1 wire.p2 strokeColor1


rowsAndColumns : Int -> (Int, Int)
rowsAndColumns n = 
  if n <= 0
  then 
    (0, 0)
  else 
    let 
      root = n |> toFloat |> sqrt
      rows = floor root
      columns = ceiling (toFloat n / toFloat rows)
    in
      (rows, columns)


spread : Float -> Float -> Int -> List Float
spread minValue maxValue count = 
  if count <= 1
  then
    [(maxValue + minValue) / 2]
  else 
    List.range 0 (count - 1)
    |> List.map toFloat
    |> List.map (\i -> i * (maxValue - minValue) / (toFloat count - 1) + minValue)


gemPositions : Int -> List (Float, Float)
gemPositions numberOfGems = 
  let 
    (rows, columns) =
      rowsAndColumns numberOfGems
    
    ys = 
      spread (minY + viewBoxMarginY) (maxY - viewBoxMarginY) rows
    
    xs = 
      spread (minX + viewBoxMarginX) (maxX - viewBoxMarginX) columns

    makeRow : Int -> Float -> List (Float, Float)
    makeRow rowIndex y =
      let 
        row = 
          List.take columns xs
          |> List.map (\ x -> (x, y))
      in 
        if modBy 2 rowIndex == 0
        then
          row
        else
          List.reverse row
  in
    ys
    |> List.take rows
    |> List.indexedMap makeRow
    |> List.concat 


isCut : Int -> Strands -> Bool
isCut index strands = 
  if index < 0
  then 
    False
  else
    case strands of 
      (head :: tail) -> 
        if List.length head == index + 1
        then 
          True
        else 
          isCut (index - List.length head) tail
      [] -> False


isYours : Bool -> Int -> Strands -> Bool
isYours yours index strands = 
  if index < 0
  then 
    not yours
  else
    case strands of 
      (head :: tail) -> 
        if List.length head > index
        then 
          yours
        else 
          isYours (not yours) (index - List.length head) tail
      [] -> not yours
