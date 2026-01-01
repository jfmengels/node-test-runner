module Test.Reporter.Json exposing (encodeFailure, reasonDecoder, reportBegin, reportComplete, reportSummary)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode exposing (Value)
import Test.Distribution exposing (DistributionReport)
import Test.Reporter.TestResults as TestResults exposing (Failure, Outcome(..), SummaryInfo)
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


reportBegin : { globs : List String, paths : List String, fuzzRuns : Int, testCount : Int, initialSeed : Int } -> Maybe Value
reportBegin { globs, paths, fuzzRuns, testCount, initialSeed } =
    Encode.object
        [ ( "event", Encode.string "runStart" )
        , ( "testCount", Encode.int testCount )
        , ( "fuzzRuns", Encode.int fuzzRuns )
        , ( "globs", Encode.list Encode.string globs )
        , ( "paths", Encode.list Encode.string paths )
        , ( "initialSeed", Encode.int initialSeed )
        ]
        |> Just


reportComplete : TestResults.TestResult -> Value
reportComplete { duration, labels, outcome } =
    Encode.object
        [ ( "event", Encode.string "testCompleted" )
        , ( "status", Encode.string (getStatus outcome) )
        , ( "labels", encodeLabels labels )
        , ( "failures", Encode.list identity (encodeFailures outcome) )
        , ( "distributionReports", Encode.list identity (encodeDistributionReports outcome) )
        , ( "duration", Encode.int duration )
        ]


encodeFailures : Outcome -> List Value
encodeFailures outcome =
    case outcome of
        Failed failures ->
            List.map (Tuple.first >> encodeFailure) failures

        Todo str ->
            [ Encode.string str ]

        Passed _ ->
            []


encodeDistributionReports : Outcome -> List Value
encodeDistributionReports outcome =
    case outcome of
        Failed failures ->
            List.map (Tuple.second >> encodeDistributionReport) failures

        Todo _ ->
            []

        Passed distributionReport ->
            [ encodeDistributionReport distributionReport ]


encodeDistributionReport : DistributionReport -> Value
encodeDistributionReport distributionReport =
    case distributionReport of
        Test.Distribution.NoDistribution ->
            Encode.null
                |> encodeSumType "NoDistribution"

        Test.Distribution.DistributionToReport r ->
            [ ( "distributionCount", encodeDistributionCount r.distributionCount )
            , ( "runsElapsed", Encode.int r.runsElapsed )
            ]
                |> Encode.object
                |> encodeSumType "DistributionToReport"

        Test.Distribution.DistributionCheckSucceeded r ->
            [ ( "distributionCount", encodeDistributionCount r.distributionCount )
            , ( "runsElapsed", Encode.int r.runsElapsed )
            ]
                |> Encode.object
                |> encodeSumType "DistributionCheckSucceeded"

        Test.Distribution.DistributionCheckFailed r ->
            [ ( "distributionCount", encodeDistributionCount r.distributionCount )
            , ( "runsElapsed", Encode.int r.runsElapsed )
            , ( "badLabel", Encode.string r.badLabel )
            , ( "badLabelPercentage", Encode.float r.badLabelPercentage )
            , ( "expectedDistribution", Encode.string r.expectedDistribution )
            ]
                |> Encode.object
                |> encodeSumType "DistributionCheckFailed"


encodeDistributionCount : Dict (List String) Int -> Value
encodeDistributionCount dict =
    dict
        |> Dict.toList
        |> Encode.list
            (\( labels, count ) ->
                Encode.object
                    [ ( "labels", Encode.list Encode.string labels )
                    , ( "count", Encode.int count )
                    ]
            )


{-| Algorithm:

  - If any fail, return "fail"
  - Otherwise, if any are todo, return "todo"
  - Otherwise, return "pass"

-}
getStatus : Outcome -> String
getStatus outcome =
    case outcome of
        Failed _ ->
            "fail"

        Todo _ ->
            "todo"

        Passed _ ->
            "pass"


encodeLabels : List String -> Value
encodeLabels labels =
    List.reverse labels
        |> Encode.list Encode.string


reportSummary : SummaryInfo -> Maybe String -> Value
reportSummary { duration, passed, failed } autoFail =
    Encode.object
        [ ( "event", Encode.string "runComplete" )
        , ( "passed", Encode.int passed )
        , ( "failed", Encode.int failed )
        , ( "duration", Encode.string <| String.fromFloat duration )
        , ( "autoFail"
          , autoFail
                |> Maybe.map Encode.string
                |> Maybe.withDefault Encode.null
          )
        ]


encodeFailure : Failure -> Value
encodeFailure { given, description, reason } =
    Encode.object
        [ ( "given", Maybe.withDefault Encode.null (Maybe.map Encode.string given) )
        , ( "message", Encode.string description )
        , ( "reason", encodeReason description reason )
        ]


encodeSumType : String -> Value -> Value
encodeSumType sumType data =
    Encode.object
        [ ( "type", Encode.string sumType )
        , ( "data", data )
        ]


encodeReason : String -> Reason -> Value
encodeReason description reason =
    case reason of
        Custom ->
            Encode.string description
                |> encodeSumType "Custom"

        Equality expected actual ->
            [ ( "expected", Encode.string expected )
            , ( "actual", Encode.string actual )
            , ( "comparison", Encode.string description )
            ]
                |> Encode.object
                |> encodeSumType "Equality"

        Comparison first second ->
            [ ( "first", Encode.string first )
            , ( "second", Encode.string second )
            , ( "comparison", Encode.string description )
            ]
                |> Encode.object
                |> encodeSumType "Comparison"

        TODO ->
            Encode.string description
                |> encodeSumType "TODO"

        Invalid BadDescription ->
            let
                explanation =
                    if description == "" then
                        "The empty string is not a valid test description."

                    else
                        "This is an invalid test description: " ++ description
            in
            Encode.string explanation
                |> encodeSumType "Invalid"

        Invalid _ ->
            Encode.string description
                |> encodeSumType "Invalid"

        ListDiff expected actual ->
            [ ( "expected", Encode.list Encode.string expected )
            , ( "actual", Encode.list Encode.string actual )
            ]
                |> Encode.object
                |> encodeSumType "ListDiff"

        CollectionDiff { expected, actual, extra, missing } ->
            [ ( "expected", Encode.string expected )
            , ( "actual", Encode.string actual )
            , ( "extra", Encode.list Encode.string extra )
            , ( "missing", Encode.list Encode.string missing )
            ]
                |> Encode.object
                |> encodeSumType "CollectionDiff"


reasonDecoder : Decoder Reason
reasonDecoder =
    Decode.field "type" Decode.string
        |> Decode.andThen
            (\sumType ->
                case sumType of
                    "Custom" ->
                        Decode.succeed Custom

                    "Equality" ->
                        Decode.map2 Equality
                            (Decode.field "expected" Decode.string)
                            (Decode.field "actual" Decode.string)

                    "Comparison" ->
                        Decode.map2 Comparison
                            (Decode.field "first" Decode.string)
                            (Decode.field "second" Decode.string)

                    "TODO" ->
                        Decode.succeed TODO

                    "Invalid" ->
                        -- TODO Handle other Invalid variants
                        Decode.succeed (Invalid BadDescription)

                    "ListDiff" ->
                        Decode.map2 ListDiff
                            (Decode.field "expected" (Decode.list Decode.string))
                            (Decode.field "actual" (Decode.list Decode.string))

                    "CollectionDiff" ->
                        Decode.map4
                            (\expected actual extra missing ->
                                CollectionDiff
                                    { expected = expected
                                    , actual = actual
                                    , extra = extra
                                    , missing = missing
                                    }
                            )
                            (Decode.field "expected" Decode.string)
                            (Decode.field "actual" Decode.string)
                            (Decode.field "extra" (Decode.list Decode.string))
                            (Decode.field "missing" (Decode.list Decode.string))

                    _ ->
                        Decode.fail ("Unknown type " ++ sumType ++ " for reason")
            )
