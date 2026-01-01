module Test.Reporter.TestResults exposing
    ( Failure
    , Outcome(..)
    , SummaryInfo
    , TestResult
    , isFailure
    , outcomesFromExpectations
    )

import Expect exposing (Expectation)
import Test.Distribution exposing (DistributionReport)
import Test.Runner
import Test.Runner.Failure exposing (Reason)


type Outcome
    = Passed DistributionReport
    | Todo String
    | Failed (List ( Failure, DistributionReport ))


type alias TestResult =
    { labels : List String
    , outcome : Outcome
    , duration : Int -- in milliseconds
    }


type alias SummaryInfo =
    { testCount : Int
    , passed : Int
    , failed : Int
    , todos : List ( List String, String )
    , duration : Float
    }


type alias Failure =
    { given : Maybe String
    , description : String
    , reason : Reason
    }


isFailure : Outcome -> Bool
isFailure outcome =
    case outcome of
        Failed _ ->
            True

        _ ->
            False


outcomesFromExpectations : List Expectation -> List Outcome
outcomesFromExpectations expectations =
    case expectations of
        expectation :: [] ->
            -- Most often we'll get exactly 1 pass, so try that case first!
            case Test.Runner.getFailureReason expectation of
                Nothing ->
                    [ Passed (Test.Runner.getDistributionReport expectation) ]

                Just failure ->
                    if Test.Runner.isTodo expectation then
                        [ Todo failure.description ]

                    else
                        [ Failed
                            [ ( failure, Test.Runner.getDistributionReport expectation ) ]
                        ]

        _ :: _ ->
            let
                builder =
                    List.foldl outcomesFromExpectationsHelp
                        { passes = [], todos = [], failures = [] }
                        expectations

                failuresList =
                    case builder.failures of
                        [] ->
                            []

                        failures ->
                            [ Failed failures ]
            in
            -- It's most likely that there will be no todos or failures,
            -- so avoid unnecessary concatenation in that case.
            if List.isEmpty builder.todos && List.isEmpty failuresList then
                builder.passes

            else
                List.concat
                    [ builder.passes
                    , builder.todos
                    , failuresList
                    ]

        [] ->
            []


type alias OutcomeBuilder =
    { passes : List Outcome
    , todos : List Outcome
    , failures : List ( Failure, DistributionReport )
    }


outcomesFromExpectationsHelp : Expectation -> OutcomeBuilder -> OutcomeBuilder
outcomesFromExpectationsHelp expectation builder =
    case Test.Runner.getFailureReason expectation of
        Just failure ->
            if Test.Runner.isTodo expectation then
                { passes = builder.passes
                , todos = Todo failure.description :: builder.todos
                , failures = builder.failures
                }

            else
                { passes = builder.passes
                , todos = builder.todos
                , failures =
                    ( failure
                    , Test.Runner.getDistributionReport expectation
                    )
                        :: builder.failures
                }

        Nothing ->
            { passes =
                Passed (Test.Runner.getDistributionReport expectation)
                    :: builder.passes
            , todos = builder.todos
            , failures = builder.failures
            }
