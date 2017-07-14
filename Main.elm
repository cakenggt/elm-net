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
    result : List Float,
    input : String,
    target : String,
    error : Float,
    backpropIter : Int
  }


--inputs [[0, 0], [1, 0], [0, 1], [1, 1]]
--targets [[0], [1], [1], [0]]
init : Navigation.Location -> (Model, Cmd Msg)
init location =
  let
    params = routeParser (Url.parsePath routeFunc location)
    inputSize = getSizeOfNestedList params.inputs
    outputSize = getSizeOfNestedList params.targets
  in
    (Model (Net.createNetDeterministic inputSize inputSize outputSize) [0] params.inputs params.targets 0 1000, Cmd.none)

routeParser : Maybe Route -> ParamsMap
routeParser route =
  let
    defaultParams = {inputs = "[[0,0],[1,0],[0,1],[1,1]]", targets = "[[0],[1],[1],[0]]"}
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
  | Forward
  | NewInput String
  | NewTarget String
  | Backprop
  | NewBackprop String
  | UrlChange Navigation.Location


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Randomize ->
      let
        inputSize = getSizeOfNestedList model.input
        outputSize = getSizeOfNestedList model.target
      in
        (model, Net.createNetRandom inputSize inputSize outputSize NewNet)

    NewNet newNet ->
      ({model | net = newNet}, Cmd.none)

    NewInput str ->
        ({model | input = str}, Cmd.none)

    NewTarget str ->
        ({model | target = str}, Cmd.none)

    Forward ->
        let
            input = withDefault [0,0] (List.head (stringToList model.input))
            target = withDefault [0] (List.head (stringToList model.target))
        in
            ({model | result = Net.forwardPass model.net input, error = Net.getTotalError model.net input target}, Cmd.none)

    Backprop ->
        let
            input = withDefault [0,0] (List.head (stringToList model.input))
            target = withDefault [0] (List.head (stringToList model.target))
        in
            ({model | net = Net.backpropagateSet model.net 1 (stringToList model.input) (stringToList model.target) model.backpropIter, error = Net.getTotalError model.net input target}, Cmd.none)

    NewBackprop str ->
        let
            iter = Result.withDefault 0 (String.toInt str)
        in
            ({model | backpropIter = iter}, Cmd.none)

    UrlChange _ ->
        (model, Cmd.none)

stringToList : String -> List (List Float)
stringToList str =
    case decodeString (Json.Decode.list (Json.Decode.list float)) str of
        Ok ls ->
            ls
        Err _ ->
            [[]]

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
  Sub.none



-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ h1 [] [ text (toString model.net) ]
    , h2 [] [ text (toString model.result)]
    , button [ onClick Randomize ] [ text "Randomize" ]
    , input [ onInput NewInput, value model.input ] []
    , input [ onInput NewTarget, value model.target ] []
    , button [ onClick Forward ] [ text "Forward" ]
    , button [ onClick Backprop ] [ text "Backprop" ]
    , input [ onInput NewBackprop, value (toString model.backpropIter) ] []
    , h2 [] [ text (toString model.error) ]
    , display model.net
    ]
