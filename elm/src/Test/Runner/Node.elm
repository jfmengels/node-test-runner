port module Test.Runner.Node exposing (check, run, TestProgram)

{-|


# Node Runner

Runs a test and outputs its results to the console. Exit code is 0 if tests
passed and 2 if any failed. Returns 1 if something went wrong.

@docs check, run, TestProgram

-}

import Array exposing (Array)
import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Platform
import Random
import Task
import Test exposing (Test)
import Test.Distribution as Distribution
import Test.Reporter.Reporter exposing (Report, RunInfo, TestReporter, createReporter)
import Test.Reporter.TestResults as TestResults exposing (Outcome, TestResult, isFailure, outcomesFromExpectations)
import Test.Runner exposing (Runner, SeededRunners(..))
import Test.Runner.JsMessage as JsMessage exposing (JsMessage(..))
import Time exposing (Posix)



-- TYPES


type alias TestId =
    Int


type alias InitArgs =
    { initialSeed : Int
    , processes : Int
    , globs : List String
    , paths : List String
    , fuzzRuns : Int
    , runners : SeededRunners
    , report : Report
    , outcomeCache : Dict (List String) (List Outcome)
    }


type alias Runner =
    { run : () -> List Outcome
    , labels : List String
    }


type alias RunnerOptions =
    { seed : Int
    , runs : Int
    , report : Report
    , globs : List String
    , paths : List String
    , processes : Int
    }


type alias Model =
    { available : Array Runner
    , runInfo : RunInfo
    , testReporter : TestReporter
    , results : List ( TestId, TestResult )
    , processes : Int
    , nextTestToRun : TestId
    , autoFail : Maybe String
    }


{-| A program which will run tests and report their results.
-}
type alias TestProgram =
    Platform.Program Int Model Msg


type Msg
    = Receive Decode.Value
    | Dispatch Posix
    | Complete (List String) (List Outcome) Posix Posix


{-| The port names are prefixed to reduce the likelihood of the project
having a port with the same name, which is a compile error.
-}
port elmTestPort__send : String -> Cmd msg


port elmTestPort__receive : (Decode.Value -> msg) -> Sub msg


dispatch : Model -> Posix -> Cmd Msg
dispatch model startTime =
    case Array.get model.nextTestToRun model.available of
        Nothing ->
            -- We're finished! Nothing left to run.
            sendResults True model.testReporter model.results

        Just config ->
            let
                outcomes =
                    config.run ()
            in
            Time.now
                |> Task.perform (Complete config.labels outcomes startTime)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ testReporter } as model) =
    case msg of
        Receive val ->
            case Decode.decodeValue JsMessage.decoder val of
                Ok (Summary duration failed todos) ->
                    let
                        testCount =
                            model.runInfo.testCount

                        summaryInfo =
                            { testCount = testCount
                            , passed = testCount - failed - List.length todos
                            , failed = failed
                            , todos = todos
                            , duration = duration
                            }

                        summary =
                            testReporter.reportSummary summaryInfo model.autoFail

                        exitCode =
                            if failed > 0 then
                                2

                            else if model.autoFail == Nothing && List.isEmpty todos then
                                0

                            else
                                3

                        cmd =
                            Encode.object
                                [ ( "type", Encode.string "SUMMARY" )
                                , ( "exitCode", Encode.int exitCode )
                                , ( "message", summary )
                                ]
                                |> Encode.encode 0
                                |> elmTestPort__send
                    in
                    ( model, cmd )

                Ok (Test index) ->
                    let
                        cmd =
                            Task.perform Dispatch Time.now
                    in
                    if index == -1 then
                        ( { model | nextTestToRun = index + model.processes }
                        , Cmd.batch [ cmd, sendBegin model ]
                        )

                    else
                        ( { model | nextTestToRun = index }, cmd )

                Err err ->
                    let
                        cmd =
                            Encode.object
                                [ ( "type", Encode.string "ERROR" )
                                , ( "message", Encode.string (Decode.errorToString err) )
                                ]
                                |> Encode.encode 0
                                |> elmTestPort__send
                    in
                    ( model, cmd )

        Dispatch startTime ->
            ( model, dispatch model startTime )

        Complete labels outcomes startTime endTime ->
            let
                duration =
                    Time.posixToMillis endTime - Time.posixToMillis startTime

                prependOutcome outcome rest =
                    ( model.nextTestToRun
                    , { labels = labels, outcome = outcome, duration = duration }
                    )
                        :: rest

                results =
                    List.foldl prependOutcome model.results outcomes

                nextTestToRun =
                    model.nextTestToRun + model.processes

                isFinished =
                    nextTestToRun >= model.runInfo.testCount
            in
            if isFinished || List.any isFailure outcomes then
                let
                    cmd =
                        sendResults isFinished testReporter results
                in
                if isFinished then
                    -- Don't bother updating the model, since we're done
                    ( model, cmd )

                else
                    -- Clear out the results, now that we've flushed them.
                    ( { model | nextTestToRun = nextTestToRun, results = [] }
                    , Cmd.batch
                        [ cmd
                        , Task.perform Dispatch Time.now
                        ]
                    )

            else
                ( { model | nextTestToRun = nextTestToRun, results = results }
                , Task.perform Dispatch Time.now
                )


sendResults : Bool -> TestReporter -> List ( TestId, TestResult ) -> Cmd msg
sendResults isFinished testReporter results =
    let
        typeStr =
            if isFinished then
                "FINISHED"

            else
                "RESULTS"

        addToKeyValues ( testId, result ) list =
            -- These are coming in in reverse order. Doing a foldl with ::
            -- means we reverse the list again, while also doing the conversion!
            ( String.fromInt testId, testReporter.reportComplete result ) :: list
    in
    Encode.object
        [ ( "type", Encode.string typeStr )
        , ( "results"
          , results
                |> List.foldl addToKeyValues []
                |> Encode.object
          )
        , ( "outcomes"
          , Encode.list
                (\( _, result ) ->
                    Encode.object
                        [ ( "labels", Encode.list Encode.string result.labels )
                        , ( "outcome", Encode.string (Debug.toString result.outcome) )
                        ]
                )
                results
          )
        ]
        |> Encode.encode 0
        |> elmTestPort__send


sendBegin : Model -> Cmd msg
sendBegin model =
    let
        baseFields =
            [ ( "type", Encode.string "BEGIN" )
            , ( "testCount", Encode.int model.runInfo.testCount )
            ]

        fields =
            case model.testReporter.reportBegin model.runInfo of
                Just report ->
                    ( "message", report ) :: baseFields

                Nothing ->
                    baseFields
    in
    Encode.object fields
        |> Encode.encode 0
        |> elmTestPort__send


init : InitArgs -> Model
init { processes, globs, paths, fuzzRuns, initialSeed, report, runners, outcomeCache } =
    let
        { availableRunners, autoFail } =
            case runners of
                Plain runnerList ->
                    { availableRunners = Array.fromList runnerList
                    , autoFail = Nothing
                    }

                Only runnerList ->
                    { availableRunners = Array.fromList runnerList
                    , autoFail = Just "Test.only was used"
                    }

                Skipping runnerList ->
                    { availableRunners = Array.fromList runnerList
                    , autoFail = Just "Test.skip was used"
                    }

                Invalid str ->
                    { availableRunners = Array.empty
                    , autoFail = Just str
                    }

        testCount =
            Array.length availableRunners

        testReporter =
            createReporter report

        availableRunnersWithCache : Array Runner
        availableRunnersWithCache =
            Array.map
                (\runner ->
                    case Dict.get runner.labels outcomeCache of
                        Just outcomes ->
                            { run = \() -> outcomes
                            , labels = runner.labels
                            }

                        Nothing ->
                            { run = \() -> outcomesFromExpectations (runner.run ())
                            , labels = runner.labels
                            }
                )
                availableRunners
    in
    { available = availableRunnersWithCache
    , runInfo =
        { testCount = testCount
        , globs = globs
        , paths = paths
        , fuzzRuns = fuzzRuns
        , initialSeed = initialSeed
        }
    , processes = processes
    , nextTestToRun = 0
    , results = []
    , testReporter = testReporter
    , autoFail = autoFail
    }


failInit : String -> Report -> Int -> ( Model, Cmd Msg )
failInit message report _ =
    let
        model =
            { available = Array.empty
            , runInfo =
                { testCount = 0
                , globs = []
                , paths = []
                , fuzzRuns = 0
                , initialSeed = 0
                }
            , processes = 0
            , nextTestToRun = 0
            , results = []
            , testReporter = createReporter report
            , autoFail = Nothing
            }

        cmd =
            Encode.object
                [ ( "type", Encode.string "SUMMARY" )
                , ( "exitCode", Encode.int 1 )
                , ( "message", Encode.string message )
                ]
                |> Encode.encode 0
                |> elmTestPort__send
    in
    ( model, cmd )


{-| The implementation of this function will be replaced in the generated JS
with a version that returns `Just value` if `value` is a `Test`, otherwise `Nothing`.

If you rename or change this function you also need to update the regex that looks for it.

-}
check : a -> Maybe Test
check =
    checkHelperReplaceMe___


checkHelperReplaceMe___ : a -> b
checkHelperReplaceMe___ _ =
    Debug.todo "The regex for replacing this Debug.todo with some real code must have failed since you see this message!\n\nPlease report this bug: https://github.com/rtfeldman/node-test-runner/issues/new\n"


{-| Run the tests.
-}
run : RunnerOptions -> List ( String, List (Maybe Test) ) -> Program Int Model Msg
run { runs, seed, report, globs, paths, processes } possiblyTests =
    let
        tests =
            possiblyTests
                |> List.filterMap
                    (\( moduleName, maybeModuleTests ) ->
                        let
                            moduleTests =
                                List.filterMap identity maybeModuleTests
                        in
                        if List.isEmpty moduleTests then
                            Nothing

                        else
                            Just (Test.describe moduleName moduleTests)
                    )
    in
    if List.isEmpty tests then
        Platform.worker
            { init = failInit (noTestsFoundError globs) report
            , update = \_ model -> ( model, Cmd.none )
            , subscriptions = \_ -> Sub.none
            }

    else
        let
            runners =
                Test.Runner.fromTest runs (Random.initialSeed seed) (Test.concat tests)

            model =
                init
                    { initialSeed = seed
                    , processes = processes
                    , globs = globs
                    , paths = paths
                    , fuzzRuns = runs
                    , runners = runners
                    , report = report
                    , outcomeCache = decodedOutcomeCache
                    }
        in
        Platform.worker
            { init = \_ -> ( model, Cmd.none )
            , update = update
            , subscriptions = \_ -> elmTestPort__receive Receive
            }


noTestsFoundError : List String -> String
noTestsFoundError globs =
    if List.isEmpty globs then
        """
No exposed values of type Test found in the tests/ directory.

Are there tests in any .elm file in the tests/ directory?
If not – add some!
If there are – are they exposed?
        """
            |> String.trim

    else
        """
No exposed values of type Test found in files matching:

%globs

Are the above patterns correct? Maybe try running elm-test with no arguments?

Are there tests in any of the matched files?
If not – add some!
If there are – are they exposed?
        """
            |> String.trim
            |> String.replace "%globs" (String.join "\n" globs)


decodedOutcomeCache : Dict (List String) (List Outcome)
decodedOutcomeCache =
    Decode.decodeString (Decode.list decodeOutcomeCacheItem) json
        |> Result.withDefault []
        |> List.foldl (\{ labels, outcome } acc -> Dict.insert labels [ outcome ] acc) Dict.empty


decodeOutcomeCacheItem : Decoder { labels : List String, outcome : Outcome }
decodeOutcomeCacheItem =
    Decode.map2 (\labels outcome -> { labels = labels, outcome = outcome })
        (Decode.field "labels" (Decode.list Decode.string))
        (Decode.field "outcome" decodeOutcome)


decodeOutcome : Decoder Outcome
decodeOutcome =
    Decode.string
        -- TODO Decode better
        |> Decode.map (\_ -> TestResults.Passed Distribution.NoDistribution)


json : String
json =
    """
[  {
     "labels": [
       "should report the correct range when exports are on multiple lines",
       "Type aliases",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an exposed type alias if it is used in a let block type annotation",
       "Type aliases",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an unused exposed custom type if it's part of the package's exposed API",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report ReviewConfig.config",
       "Functions and values",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an exposed function when it is used in other modules (using `exposing` to import)",
       "Functions and values",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report a type alias that's used externally",
       "When exposing all",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "reports an unused function (followed by a type declaration)",
       "When exposing all",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report elements from ignored modules used in other ignored modules exposed tests even if they're in an ignored module",
       "reportUnusedProductionExports",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report the `ReviewConfig` module",
       "When module is never imported",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an export if it is imported by name",
       "Imports",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an unused exposed type alias if it's present in the signature of an exposed function",
       "Type aliases",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an unused exposed custom type if it's present in an exposed type alias (nested)",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report a used exposed custom type (value usage)",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should report an exposed function when it is not used in other modules, even if it is used in the module",
       "Functions and values",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not remove a type alias used in a local let binding type annotation",
       "When exposing all",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "reports an unused custom type",
       "When exposing all",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should report elements never used anywhere even if they're annotated with a tag",
       "reportUnusedProductionExports",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should report non-exposed and non-used package modules that expose a `main` function",
       "When module is never imported",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an exposed `app` function in Lamdera applications",
       "Lamdera support",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an unused exposed type alias if it's present in an exposed type alias (nested)",
       "Type aliases",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an unused exposed custom type if it's present in an exposed custom type constructor's arguments (nested)",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report a used exposed custom type (function declaration destructuring)",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report the `main` function for an application even if it is unused",
       "Functions and values",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "does not report a port that's used internally",
       "When exposing all",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "reports an unused function",
       "When exposing all",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should report functions that are only used in ignored files (helpers defined)",
       "reportUnusedProductionExports",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report a module with main function if we don't know the project type",
       "When module is never imported",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an exposed type if it is used in a port (output)",
       "Type aliases",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report a used exposed type alias (used in type alias)",
       "Type aliases",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an unused exposed custom type if it's aliased by an exposed type alias",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should report an unused exposed custom type",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an exposed value when it is used in other modules (using record update syntax, importing explicitly)",
       "Functions and values",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report a custom type that's used externally",
       "When exposing all",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "reports an unused recursive function",
       "When exposing all",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report elements only used in ignored modules if they're annotated with a tag",
       "reportUnusedProductionExports",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report modules exposed in a package",
       "When module is never imported",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should report an exposed `app` function in packages",
       "Lamdera support",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an unused exposed type alias if it's aliased by an exposed type alias",
       "Type aliases",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report an unused exposed custom type if it's present in an exposed custom type constructor's arguments",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should not report a used exposed custom type (case expression usage)",
       "Types",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   },
   {
     "labels": [
       "should propose a fix for unused exports if there are others exposed elements",
       "Functions and values",
       "NoUnusedExports",
       "NoUnused.ExportsTest"
     ],
     "outcome": "Passed NoDistribution"
   }
]"""
