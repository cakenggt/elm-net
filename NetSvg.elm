module NetSvg exposing (display)
import Net
import Html
import Svg exposing (..)
import Svg.Attributes exposing (..)

type alias Coord = (Float, Float)

display : Net.Net -> Html.Html msg
display net =
  let
    size = 1000
    sizeStr = (toString size)
  in
    svg
      [ width sizeStr, height sizeStr, viewBox ("0 0 " ++ sizeStr ++ " " ++ sizeStr) ]
      (List.concat
      [ generateNodeCircles net size size
      , generateWeightLines net size size
      ])

generateNodeCircles : Net.Net -> Int -> Int -> List (Svg a)
generateNodeCircles net width height =
  let
    nodeWidth = getNodeWidth net width
    circleGenerator = generateSingleNodeCircle nodeWidth
  in
    List.map circleGenerator (generateNodePositions net width height)

getNodeWidth : Net.Net -> Int -> Float
getNodeWidth net width =
  let
    maxNodes = Maybe.withDefault 1 (List.maximum [net.inputs, net.hidden, net.outputs, 3])
  in
    (toFloat width)/((toFloat maxNodes)*2+1)

generateNodePositions : Net.Net -> Int -> Int -> List Coord
generateNodePositions net width height =
  let
    nodeWidth = getNodeWidth net width
    padding = 1.5*nodeWidth
    centerBoxWidth = (toFloat width)-(2*padding)
    centerBoxHeight = (toFloat height)-(2*padding)
  in
    List.concat
    [ (List.map (\i ->
        generateSingleNodePosition padding (1.5*nodeWidth) centerBoxHeight i net.inputs) (List.range 0 (net.inputs-1)))
    , (List.map (\i ->
        generateSingleNodePosition padding (3.5*nodeWidth) centerBoxHeight i net.hidden) (List.range 0 (net.hidden-1)))
    , (List.map (\i ->
        generateSingleNodePosition padding (5.5*nodeWidth) centerBoxHeight i net.outputs) (List.range 0 (net.outputs-1)))
    ]

generateSingleNodePosition : Float -> Float -> Float -> Int -> Int -> Coord
generateSingleNodePosition padding xpos centerBoxHeight index total =
  let
    ifloat = toFloat index
    vertSpacing = centerBoxHeight/(toFloat total)
    centerPadding = vertSpacing / 2
    ypos = padding+centerPadding+(ifloat*vertSpacing)
  in
    (xpos, ypos)

generateSingleNodeCircle : Float -> Coord -> Svg a
generateSingleNodeCircle nodeWidth (xpos, ypos) =
  let
    radius = toString (nodeWidth/2)
  in
    circle [ cx (toString xpos), cy (toString ypos), r radius] []

generateWeightLines : Net.Net -> Int -> Int -> List (Svg a)
generateWeightLines net width height =
  let
    inputWeights = List.take (net.inputs*net.hidden) net.weights
    outputWeights = List.drop (net.inputs*net.hidden) net.weights
    nodePositions = generateNodePositions net width height
    inputs = List.take net.inputs nodePositions
    hiddens = List.take net.hidden (List.drop net.inputs nodePositions)
    outputs = List.drop (net.inputs+net.hidden) nodePositions
    nodeWidth = getNodeWidth net width
  in
    List.concat
    [ generateWeightLinesForSection inputWeights inputs hiddens nodeWidth
    , generateWeightLinesForSection outputWeights hiddens outputs nodeWidth
    ]

generateWeightLinesForSection : List Float -> List Coord -> List Coord -> Float -> List (Svg a)
generateWeightLinesForSection weights fromCoords toCoords nodeWidth =
  List.map2 (generateWeightLine (nodeWidth/2 + 10))
    (List.concat (List.map (\from -> List.map (\to -> (from, to)) toCoords) fromCoords))
    weights

generateWeightLine : Float -> (Coord, Coord) -> Float -> Svg a
generateWeightLine shorten ((fromx, fromy), (tox, toy)) weight =
  let
    color = if weight < 0 then "grey" else "black"
    c2 = shorten^2
    slope = (toy-fromy)/(tox-fromx)
    diffx = sqrt (c2/(1+slope^2))
    diffy = slope * diffx
    toy2 = toy - diffy
    tox2 = tox - diffx
    fromy2 = fromy + diffy
    fromx2 = fromx + diffx
    width = abs (((((Net.logistic weight) * 2) - 1) * 15) + 2)
  in
    line [ x1 (toString fromx2), y1 (toString fromy2), x2 (toString tox2), y2 (toString toy2), strokeWidth (toString width), stroke color] []
