-- Read more about this program in the official Elm guide:
-- https://guide.elm-lang.org/architecture/effects/random.html

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Net
import Json.Decode exposing (decodeString, list, float)
import NetSvg exposing (display)
import Maybe exposing (..)
import Navigation
import UrlParser as Url exposing ((</>), (<?>), s, int, stringParam, top, string)
import Http exposing (encodeUri, decodeUri)
import Time exposing (Time, millisecond)


main : Program Never Model Msg
main =
  Navigation.program UrlChange
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

type alias ParamsMap = {inputs : String, targets : String}

type Route
    = Params String (Maybe String) (Maybe String) -- needs the string at the beginning due to elm-reactor

routeFunc : Url.Parser (Route -> a) a
routeFunc =
  Url.oneOf
    [ Url.map Params (string <?> stringParam "inputs" <?> stringParam "targets")
    ]


-- MODEL


type alias Model =
  { net : Net.Net,
    tests : List (List String, List String),
    inputs : Int,
    hiddens : Int,
    outputs : Int,
    backpropIter : Int,
    running : Bool
  }


--inputs [[0, 0], [1, 0], [0, 1], [1, 1]]
--targets [[0], [1], [1], [0]]
init : Navigation.Location -> (Model, Cmd Msg)
init location =
  let
    params = routeParser (Url.parsePath routeFunc location)
    tests = List.map2 (,) (stringToList params.inputs) (stringToList params.targets)
    inputSize = getSizeOfNestedList params.inputs
    outputSize = getSizeOfNestedList params.targets
  in
    (Model (Net.createNetDeterministic inputSize inputSize outputSize) tests inputSize inputSize outputSize 1000 False, Cmd.none)

routeParser : Maybe Route -> ParamsMap
routeParser route =
  let
    defaultParams = {inputs = "[[\"0\",\"0\"],[\"1\",\"0\"],[\"0\",\"1\"],[\"1\",\"1\"]]", targets = "[[\"0\"],[\"1\"],[\"1\"],[\"0\"]]"}
  in
    case route of
      Just params ->
        case params of
          Params str maybeInputs maybeTargets ->
            {inputs = (maybeUri defaultParams.inputs maybeInputs), targets = (maybeUri defaultParams.targets maybeTargets)}
      Nothing ->
        defaultParams

maybeUri : String -> Maybe String -> String
maybeUri default maybe =
  case maybe of
    Just uri ->
      withDefault default (decodeUri uri)
    Nothing ->
      default


-- UPDATE


type Msg
  = Randomize
  | NewNet Net.Net
  | Backprop
  | TimedBackprop Time
  | NewBackprop String
  | UrlChange Navigation.Location
  | IncInput
  | DecInput
  | IncHidden
  | DecHidden
  | IncOutput
  | DecOutput
  | ChangeTest Int NodeType Int String
  | ToggleRunning
  | AddTest
  | RemoveTest Int

type NodeType
  = Input
  | Hidden
  | Output


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Randomize ->
      (model, Net.createNetRandom model.inputs model.hiddens model.outputs NewNet)

    NewNet newNet ->
      ({model | net = newNet}, Cmd.none)

    Backprop ->
      let
        tests = testsToFloats model.tests
      in
        ( {model | net = Net.backpropagateSet model.net 0.1 tests model.backpropIter}
        , Cmd.none)

    TimedBackprop time ->
      let
        tests = testsToFloats model.tests
      in
        if model.running then
          ( {model | net = Net.backpropagateSet model.net 1 tests 1}
          , Cmd.none)
        else
          (model, Cmd.none)

    NewBackprop str ->
      let
        iter = Result.withDefault 0 (String.toInt str)
      in
        ({model | backpropIter = iter}, Cmd.none)

    UrlChange _ ->
        (model, Cmd.none)

    IncInput ->
      ({model
      | inputs = model.inputs + 1
      , tests = List.map (\(input, output) -> (List.append input ["0"], output)) model.tests}
      , Net.createNetRandom (model.inputs + 1) model.hiddens model.outputs NewNet)

    DecInput ->
      let
        newNum = model.inputs - 1
        finalNum = if newNum < 1 then 1 else newNum
        tests = if newNum < 1 then model.tests else List.map (\(input, output) -> (pop input, output)) model.tests
      in
        ({model
        | inputs = finalNum
        , tests = tests}
        , Net.createNetRandom finalNum model.hiddens model.outputs NewNet)

    IncHidden ->
      ({model
      | hiddens = model.hiddens + 1}
      , Net.createNetRandom model.inputs (model.hiddens + 1) model.outputs NewNet)

    DecHidden ->
      let
        newNum = model.hiddens - 1
        finalNum = if newNum < 1 then 1 else newNum
      in
        ({model
        | hiddens = finalNum}
        , Net.createNetRandom model.inputs finalNum model.outputs NewNet)

    IncOutput ->
      ({model
      | outputs = model.outputs + 1
      , tests = List.map (\(input, output) -> (input, List.append output ["0"])) model.tests}, Net.createNetRandom model.inputs model.hiddens (model.outputs + 1) NewNet)

    DecOutput ->
      let
        newNum = model.outputs - 1
        finalNum = if newNum < 1 then 1 else newNum
        tests = if newNum < 1 then model.tests else List.map (\(input, output) -> (input, pop output)) model.tests
      in
        ({model
        | outputs = finalNum
        , tests = tests}, Net.createNetRandom model.inputs model.hiddens finalNum NewNet)

    ChangeTest testIndex nodeType nodeIndex str ->
      let
        tests = List.indexedMap (\index test ->
          if index == testIndex then
            case nodeType of
              Input ->
                (List.indexedMap (\index node ->
                  if index == nodeIndex then
                    str
                  else
                    node) (Tuple.first test), Tuple.second test)
              Hidden ->
                test
              Output ->
                (Tuple.first test, List.indexedMap (\index node ->
                  if index == nodeIndex then
                    str
                  else
                    node) (Tuple.second test))
          else
            test) model.tests
      in
        ({model | tests = tests}, Cmd.none)

    ToggleRunning ->
      ({model | running = not model.running}, Cmd.none)

    AddTest ->
      ({model
      | tests = List.append model.tests [(List.repeat model.inputs "0", List.repeat model.outputs "0")]}
      , Cmd.none)

    RemoveTest index ->
      ({model
      | tests = List.filterMap (\(i, test) -> if i /= index then Just test else Nothing) (List.indexedMap (,) model.tests)}
      , Cmd.none)

pop : List a -> List a
pop ls =
  List.take ((List.length ls)-1) ls


stringToList : String -> List (List String)
stringToList str =
  case decodeString (Json.Decode.list (Json.Decode.list Json.Decode.string)) str of
    Ok ls ->
      ls
    Err _ ->
      [[]]

testsToFloats : List (List String, List String) -> List (List Float, List Float)
testsToFloats test =
  List.map (\(input, output) ->
    (testToFloat input
    ,testToFloat output)) test

testToFloat : List String -> List Float
testToFloat test =
  List.map (\s -> Result.withDefault 0 (String.toFloat s)) test

getSizeOfNestedList : String -> Int
getSizeOfNestedList str =
  let
    total = stringToList str
  in
    case List.head total of
      Just ls ->
        List.length ls
      Nothing ->
        0

-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every millisecond TimedBackprop



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ button [ onClick Randomize ] [ text "Randomize" ]
    , createNetDimensions model
    , button [ onClick Backprop ] [ text "Backprop" ]
    , input [ onInput NewBackprop, value (toString model.backpropIter) ] []
    , createTests model
    , div [onClick ToggleRunning] [ text (if model.running then "Pause" else "Start")]
    , display model.net
    ]

createNetDimensions : Model -> Html Msg
createNetDimensions model =
  div []
    [ (createNetDimension model.inputs IncInput DecInput)
    , (createNetDimension model.hiddens IncHidden DecHidden)
    , (createNetDimension model.outputs IncOutput DecOutput)]

createNetDimension : Int -> Msg -> Msg -> Html Msg
createNetDimension current incMsg decMsg =
  div []
    [ div [ onClick incMsg ] [ text "inc" ]
    , div [] [ text (toString current) ]
    , div [ onClick decMsg ] [ text "dec" ]]

createTests : Model -> Html Msg
createTests model =
  div [] (List.append
  (List.indexedMap (\index test -> createTest model index test) model.tests)
  [span [onClick AddTest] [text "+"]])

createTest : Model -> Int -> (List String, List String) -> Html Msg
createTest model testIndex test =
  div []
    (List.concat
    [ (List.indexedMap (\nodeIndex input -> createTestInput model testIndex Input nodeIndex input) (Tuple.first test))
    , (List.indexedMap (\nodeIndex input -> createTestInput model testIndex Output nodeIndex input) (Tuple.second test))
    , [text (toString (Net.forwardPass model.net (testToFloat (Tuple.first test))))]
    , [span [onClick (RemoveTest testIndex)] [text "-"]]])

createTestInput : Model -> Int -> NodeType -> Int -> String -> Html Msg
createTestInput model testIndex nodeType nodeIndex val =
  input [ onInput (ChangeTest testIndex nodeType nodeIndex), value val] []
