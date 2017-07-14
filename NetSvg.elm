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
    in
        List.concat
        [ generateWeightLinesForSection inputWeights inputs hiddens
        , generateWeightLinesForSection outputWeights hiddens outputs
        ]

generateWeightLinesForSection : List Float -> List Coord -> List Coord -> List (Svg a)
generateWeightLinesForSection weights fromCoords toCoords =
    List.map2 generateWeightLine
        (List.concat (List.map (\from -> List.map (\to -> (from, to)) toCoords) fromCoords))
        weights

generateWeightLine : (Coord, Coord) -> Float -> Svg a
generateWeightLine ((fromx, fromy), (tox, toy)) weight =
    let
        color = if weight < 0 then "grey" else "black"
    in
        line [ x1 (toString fromx), y1 (toString fromy), x2 (toString tox), y2 (toString toy), strokeWidth (toString (abs (weight*2))), stroke color] []
