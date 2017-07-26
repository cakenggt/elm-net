module Net
    exposing
        ( Net
        , TrainingSet
        , backpropagateSet
        , createNetDeterministic
        , createNetRandom
        , forwardPass
        )

{-| This library provides a Hidden Layer Neural Network,
backpropagation training, and a forward pass function for its' use.


# Definition

@docs Net, TrainingSet


# Helpers

@docs createNetDeterministic, createNetRandom


# Training and Forward Pass

@docs backpropagateSet, forwardPass

-}

import Random


{-| Represents a Hidden Layer Neural Net. This type should not be instantiated
directly, but instead created with `createNetDeterministic` or `createNetRandom`.
-}
type alias Net =
    { inputs : Int
    , hidden : Int
    , outputs : Int
    , weights : List Float --weights go from i0->h0, i0->h1 ... im->hn, h0->o0, h0->o1 ... hn->ox
    }


{-| Represents a training set for a Neural Net,
containing both the inputs and the targets that should
be output for those inputs.

    -- This would be the training set list for XOR
    [ TrainingSet [0,0] [0]
    , TrainingSet [0,1] [1]
    , TrainingSet [1,0] [1]
    , TrainingSet [1,1] [0]]

-}
type alias TrainingSet =
    { inputs : List Float
    , targets : List Float
    }


{-| Creates a Neural Net with the specified number of input, hidden,
and output nodes. A seed is provided so the Random library can populate
the pseudo-random initial weights. If a more random Neural Net is required,
use the `createNetRandom` method.

    -- Creates a Neural Net with 2 input nodes, 2 hidden nodes, and 1 output node
    createNetDeterministic 2 2 1 547623465437

-}
createNetDeterministic : Int -> Int -> Int -> Int -> Net
createNetDeterministic input hidden output seed =
    { inputs = input
    , hidden = hidden
    , outputs = output
    , weights = Tuple.first (Random.step (Random.list (input * hidden + hidden * output) (Random.float 0 1)) (Random.initialSeed seed))
    }


{-| Creates a Neural Net with the specified number of input, hidden,
and output nodes. A command is returned, which will use the provided
message generation function to give the Neural Net to your update function.
If a Neural Net is needed immediately without a command, use `createNetDeterministic`.

    init : ( Net, Cmd Msg )
    init =
        createNetDeterministic 2 2 1 547623465437 ! [ createNetRandom 2 2 1 NewNet ]

    update : Msg -> Model -> ( Model, Cmd Msg )
    update msg model =
        case msg of
            NewNet net ->
                net ! []

-}
createNetRandom : Int -> Int -> Int -> (Net -> msg) -> Cmd msg
createNetRandom input hidden output msg =
    Random.generate (assembleNet input hidden output msg) (Random.list (input * hidden + hidden * output) (Random.float 0 1))


assembleNet : Int -> Int -> Int -> (Net -> msg) -> List Float -> msg
assembleNet input hidden output msg weights =
    msg
        { inputs = input
        , hidden = hidden
        , outputs = output
        , weights = weights
        }



--Goes from i0->h0, i0->h1, i0->h2 ... im->hn-1, im->hn


forwardToHidden : Net -> List Float -> List Float
forwardToHidden net inputs =
    let
        numHidden =
            net.hidden

        weightsToHidden =
            getWeightsToHidden net

        weightsGroupedByHidden =
            splitListByMod weightsToHidden numHidden
    in
    List.map (\inner -> logistic (List.foldr (+) 0 (List.map2 (*) inner inputs))) weightsGroupedByHidden



--Goes from h0->o0, h0->o1, h0->o2 ... hm->on-1, hm->on


forwardToOutput : Net -> List Float -> List Float
forwardToOutput net hiddens =
    let
        weightsToOutput =
            getWeightsToOutput net

        weightsGroupedByOutput =
            splitListByMod weightsToOutput net.outputs
    in
    List.map (\inner -> logistic (List.foldr (+) 0 (List.map2 (*) inner hiddens))) weightsGroupedByOutput



--The complete forward pass is done in this function, returning the outputs


{-| Performs a forward pass on a Neural Net with
the provided set of input node values. Returns the
output node values.

    -- net is a Neural Net with 2 inputs and 1 output
    forwardPass net [1, 0] == [0.346433]

-}
forwardPass : Net -> List Float -> List Float
forwardPass net inputs =
    forwardToOutput net (forwardToHidden net inputs)


getWeightsToOutput : Net -> List Float
getWeightsToOutput net =
    let
        numHidden =
            net.hidden

        numInputs =
            net.inputs
    in
    List.drop (numHidden * numInputs) net.weights


getWeightsToHidden : Net -> List Float
getWeightsToHidden net =
    let
        numHidden =
            net.hidden

        numInputs =
            net.inputs
    in
    List.take (numHidden * numInputs) net.weights


logistic : Float -> Float
logistic input =
    1 / (1 + e ^ -input)


{-| Splits a list up into multiple sublists which are
the specified length long, at most. The sublists will
contain the elements in the original list in order.

    splitList [1,2,3,4,5] 2 == [[1,2],[3,4],[5]]

-}
splitList : List a -> Int -> List (List a)
splitList input size =
    let
        numLists =
            floor ((input |> List.length |> toFloat) / toFloat size)
    in
    List.map (\i -> List.take size (List.drop (size * i) input)) (List.range 0 (numLists - 1))


{-| Splits a list up into multiple sublists whose
elements' indexes all shared the same mod value
when divided modulus by the provided number. The number
of sublists will be at most the given number long.

    splitListByMod [1,2,3,4,5] 2 == [[1,3,5],[2,4]]

-}
splitListByMod : List a -> Int -> List (List a)
splitListByMod input size =
    let
        indexedList =
            List.indexedMap (\index item -> ( index, item )) input
    in
    List.map
        (\i ->
            List.filterMap
                (\( index, element ) ->
                    if index % size == i then
                        Just element
                    else
                        Nothing
                )
                indexedList
        )
        (List.range 0 (size - 1))


getNdOutputs : List Float -> List Float -> List Float
getNdOutputs outputs targets =
    List.map2 (\target out -> -(target - out) * (out * (1 - out))) targets outputs


getOutputWeightDeltas : List Float -> List Float -> List Float
getOutputWeightDeltas hiddenOutputs ndOutputs =
    List.concat
        (List.map
            (\hiddenOutput ->
                List.map
                    (\ndOutput ->
                        ndOutput * hiddenOutput
                    )
                    ndOutputs
            )
            hiddenOutputs
        )



--There is one of these for every hidden node


generateHiddenPartialDeltas : List (List Float) -> List Float -> List Float -> List Float
generateHiddenPartialDeltas splitHiddenWeights hiddenOutputs ndOutputs =
    List.map2
        (\out weights ->
            List.foldr (+)
                0
                (List.map2 (*) ndOutputs weights)
                * (out * (1 - out))
        )
        hiddenOutputs
        splitHiddenWeights


getInputWeightDeltas : List (List Float) -> List Float -> List Float -> List Float -> List Float
getInputWeightDeltas splitHiddenWeights hiddenOutputs ndOutputs inputs =
    let
        hiddenPartialDeltas =
            generateHiddenPartialDeltas splitHiddenWeights hiddenOutputs ndOutputs
    in
    List.concat
        (List.map
            (\input ->
                List.map (\delta -> delta * input) hiddenPartialDeltas
            )
            inputs
        )


getWeightDeltas : List (List Float) -> List Float -> List Float -> List Float -> List Float
getWeightDeltas splitHiddenWeights hiddenOutputs ndOutputs inputs =
    List.concat
        [ getInputWeightDeltas splitHiddenWeights hiddenOutputs ndOutputs inputs
        , getOutputWeightDeltas hiddenOutputs ndOutputs
        ]


adjustWeights : Net -> List Float -> Float -> Net
adjustWeights net weightDeltas learning =
    { net
        | weights =
            List.map2
                (\weight delta ->
                    weight - (learning * delta)
                )
                net.weights
                weightDeltas
    }


getTotalError : Net -> List Float -> List Float -> Float
getTotalError net inputs targets =
    let
        outputs =
            forwardPass net inputs
    in
    List.foldr (+) 0 (List.map2 (\output target -> 0.5 * (target - output) ^ 2) outputs targets)


backpropagateSingle : Net -> Float -> List Float -> List Float -> Int -> Net
backpropagateSingle net learning inputs targets iterations =
    if iterations > 0 then
        let
            hiddenWeights =
                List.drop (net.hidden * net.inputs) net.weights

            splitHiddenWeights =
                splitList hiddenWeights net.outputs

            hiddenOutputs =
                forwardToHidden net inputs

            outputs =
                forwardToOutput net hiddenOutputs

            ndOutputs =
                getNdOutputs outputs targets

            weightDeltas =
                getWeightDeltas splitHiddenWeights hiddenOutputs ndOutputs inputs
        in
        backpropagateSingle (adjustWeights net weightDeltas learning) learning inputs targets (iterations - 1)
    else
        net


{-| Performs a backpropagation training on a Neural Net,
returning the trained Neural Net. The net is provided, as is
the learning rate, training set, and number of times to train.

    backpropagateSet net 0.5 [TrainingSet [1,0] [1]] 1000 /= net

-}
backpropagateSet : Net -> Float -> List TrainingSet -> Int -> Net
backpropagateSet net learning trainingSets iterations =
    if iterations > 0 then
        backpropagateSet
            (List.foldr (\training net -> backpropagateSingle net learning training.inputs training.targets 1) net trainingSets)
            learning
            trainingSets
            (iterations - 1)
    else
        net
