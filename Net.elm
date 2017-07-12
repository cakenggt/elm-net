module Net exposing (..)
import Random
import Debug

type alias Net =
    {
    inputs : Int,
    hidden : Int,
    outputs: Int,
    weights: List Float --weights go from i0->h0, i0->h1 ... im->hn, h0->o0, h0->o1 ... hn->ox
    }

createNetDeterministic : Int -> Int -> Int -> Net
createNetDeterministic input hidden output =
    {
        inputs = input,
        hidden = hidden,
        outputs = output,
        weights = Tuple.first (Random.step (Random.list (input*hidden + hidden*output) (Random.float 0 1)) (Random.initialSeed 73464356))
    }

createNetRandom : Int -> Int -> Int -> (Net -> msg) -> Cmd msg
createNetRandom input hidden output msg =
    Random.generate (assembleNet input hidden output msg) (Random.list (input*hidden + hidden*output) (Random.float 0 1))

assembleNet : Int -> Int -> Int -> (Net -> msg) -> List Float -> msg
assembleNet input hidden output msg weights =
    msg {
        inputs = input,
        hidden = hidden,
        outputs = output,
        weights = weights
    }

--Goes from i0->h0, i0->h1, i0->h2 ... im->hn-1, im->hn
forwardToHidden : Net -> List Float -> List Float
forwardToHidden net inputs =
    let
        numHidden = net.hidden
        weightsToHidden = getWeightsToHidden net
        weightsGroupedByHidden = splitListByMod weightsToHidden numHidden
    in
        List.map (\inner -> logistic (List.foldr (+) 0 (List.map2 (*) inner inputs))) weightsGroupedByHidden

--Goes from h0->o0, h0->o1, h0->o2 ... hm->on-1, hm->on
forwardToOutput : Net -> List Float -> List Float
forwardToOutput net hiddens =
    let
        weightsToOutput = getWeightsToOutput net
        weightsGroupedByOutput = splitListByMod weightsToOutput net.outputs
    in
        List.map (\inner -> logistic (List.foldr (+) 0 (List.map2 (*) inner hiddens))) weightsGroupedByOutput

--The complete forward pass is done in this function, returning the outputs
forwardPass : Net -> List Float -> List Float
forwardPass net inputs =
    forwardToOutput net (forwardToHidden net inputs)

getWeightsToOutput : Net -> List Float
getWeightsToOutput net =
    let
        numHidden = net.hidden
        numInputs = net.inputs
    in
        List.drop (numHidden * numInputs) net.weights

getWeightsToHidden : Net -> List Float
getWeightsToHidden net =
    let
        numHidden = net.hidden
        numInputs = net.inputs
    in
        List.take (numHidden * numInputs) net.weights

logistic : Float -> Float
logistic input =
    1 / (1 + e^(-input))

splitList : List a -> Int -> List(List a)
splitList input size =
    let
        numLists = (floor ((input |> List.length |> toFloat)/(toFloat size)))
    in
        List.map (\i -> List.take size (List.drop (size * i) input)) (List.range 0 (numLists-1))

--Splits a list by the index mod a certain number, grouping all the same mods together
splitListByMod : List a -> Int -> List(List a)
splitListByMod input size =
    let
        indexes = List.range 0 ((List.length input)-1)
        indexedList = List.map2 (\index item -> (index, item)) indexes input
    in
        List.map (\i -> List.filterMap (\(index, element) ->
            if index%size == i then
                Just element
            else
                Nothing) indexedList) (List.range 0 (size-1))

getNdOutputs : List Float -> List Float -> List Float
getNdOutputs outputs targets =
    List.map2 (\target out -> -(target - out)*(out*(1-out))) targets outputs

getOutputWeightDeltas : List Float -> List Float -> List Float
getOutputWeightDeltas hiddenOutputs ndOutputs =
    List.concat
        (List.map (\hiddenOutput ->
            List.map (\ndOutput ->
                ndOutput * hiddenOutput)
            ndOutputs)
        hiddenOutputs)

--There is one of these for every hidden node
generateHiddenPartialDeltas : List(List Float) -> List Float -> List Float -> List Float
generateHiddenPartialDeltas splitHiddenWeights hiddenOutputs ndOutputs =
    List.map2 (\out weights ->
        (List.foldr (+) 0
            (List.map2 (*) ndOutputs weights))*(out*(1-out)))
    hiddenOutputs splitHiddenWeights

getInputWeightDeltas : List(List Float) -> List Float -> List Float -> List Float -> List Float
getInputWeightDeltas splitHiddenWeights hiddenOutputs ndOutputs inputs =
    let
        hiddenPartialDeltas = generateHiddenPartialDeltas splitHiddenWeights hiddenOutputs ndOutputs
    in
        List.concat (List.map (\input ->
            List.map (\delta -> delta * input) hiddenPartialDeltas)
        inputs)

getWeightDeltas : List(List Float) -> List Float -> List Float -> List Float -> List Float
getWeightDeltas splitHiddenWeights hiddenOutputs ndOutputs inputs =
    List.concat [
        (getInputWeightDeltas splitHiddenWeights hiddenOutputs ndOutputs inputs),
        (getOutputWeightDeltas hiddenOutputs ndOutputs)
    ]

adjustWeights : Net -> List Float -> Float -> Net
adjustWeights net weightDeltas learning =
    {net | weights = List.map2 (\weight delta ->
        weight - (learning * delta))
    net.weights weightDeltas}

getTotalError : Net -> List Float -> List Float -> Float
getTotalError net inputs targets =
    let
        outputs = forwardPass net inputs
    in
        List.foldr (+) 0 (List.map2 (\output target -> 0.5*(target - output)^2) outputs targets)

backpropagateSingle : Net -> Float -> List Float -> List Float -> Int -> Net
backpropagateSingle net learning inputs targets iterations =
    if iterations > 0 then
        let
            hiddenWeights = List.drop (net.hidden * net.inputs) net.weights
            splitHiddenWeights = splitList hiddenWeights net.outputs
            hiddenOutputs = forwardToHidden net inputs
            outputs = forwardToOutput net hiddenOutputs
            ndOutputs = getNdOutputs outputs targets
            weightDeltas = getWeightDeltas splitHiddenWeights hiddenOutputs ndOutputs inputs
        in
            backpropagateSingle (adjustWeights net weightDeltas learning) learning inputs targets (iterations - 1)
    else
        net

backpropagateSet : Net -> Float -> List (List Float) -> List (List Float) -> Int -> Net
backpropagateSet net learning inputs targets iterations =
    if iterations > 0 then
        let
            trainingTuples = List.map2 (\input target -> (input, target)) inputs targets
        in
            backpropagateSet
                (List.foldr (\training net -> backpropagateSingle net learning (Tuple.first training) (Tuple.second training) 1) net trainingTuples)
                learning inputs targets (iterations-1)
    else
        net
