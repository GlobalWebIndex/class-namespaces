module Main exposing (main)

import Benchmark exposing (..)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import Escape
import Old.Escape


main : BenchmarkProgram
main =
    program <|
        describe "sanitization"
            [ sanitizeBenchmark "input is all valid" valid
            , sanitizeBenchmark "input has invalid stuff" invalid
            ]


sanitizeBenchmark : String -> String -> Benchmark
sanitizeBenchmark description input =
    Benchmark.compare description
        "Old.Escape.sanitize"
        (\_ -> Old.Escape.sanitize input)
        "Escape.sanitize"
        (\_ -> Escape.sanitize input)


valid =
    "crosstab-builder"


invalid =
    "  98 hel ___ žščř--ďťň  lo"
