module Main exposing (..)

import Benchmark exposing (Benchmark, describe, scale)
import Benchmark.Runner exposing (BenchmarkProgram, program)
import BoundingBox2d exposing (BoundingBox2d)
import Debug exposing (toString)
import Length exposing (Meters)
import SpatialIndex.SpatialIndex2D as SpatialIndex exposing (SpatialIndex)


main : BenchmarkProgram
main =
    program suite


suite : Benchmark
suite =
    describe "SpatialIndex Benchmarks"
        [ insert
        , collisions
        , partition
        ]


type WorldCoordinates
    = Never


prng : Int -> Int -> Int -> Int -> Int
prng multiplier modulo increment seed =
    (multiplier * seed + increment) |> modBy modulo


createBounds : Int -> BoundingBox2d Meters WorldCoordinates
createBounds n =
    BoundingBox2d.fromExtrema
        { minX = Length.meters (prng -3 19 4 n |> toFloat |> (/) 10)
        , maxX = Length.meters (prng 3 23 7 n |> toFloat |> (/) 10)
        , minY = Length.meters (prng -5 31 3 n |> toFloat |> (/) 10)
        , maxY = Length.meters (prng 3 37 8 n |> toFloat |> (/) 10)
        }


indexOfSize : Int -> SpatialIndex Meters WorldCoordinates Int
indexOfSize size =
    List.range 0 size
        |> List.map (\s -> ( size, createBounds s ))
        |> List.map (\( s, bounds ) -> SpatialIndex.element bounds s)
        |> SpatialIndex.fromElements


insert : Benchmark
insert =
    let
        testElement =
            SpatialIndex.element (createBounds -1) -1
    in
    List.range 0 8
        |> List.map ((^) 2)
        |> List.map (\size -> ( size, indexOfSize size ))
        |> List.map (\( size, target ) -> ( toString size, \_ -> SpatialIndex.insert testElement target ))
        |> scale "SpatialIndex.insert"


collisions : Benchmark
collisions =
    List.range 0 8
        |> List.map ((^) 2)
        |> List.map (\size -> ( size, indexOfSize size ))
        |> List.map (\( size, target ) -> ( toString size, \_ -> SpatialIndex.collisions target ))
        |> scale "SpatialIndex.collisions"


partition : Benchmark
partition =
    let
        range =
            createBounds -1
    in
    List.range 0 8
        |> List.map ((^) 2)
        |> List.map (\size -> ( size, indexOfSize size ))
        |> List.map (\( size, target ) -> ( toString size, \_ -> SpatialIndex.partitionIntersecting range target ))
        |> scale "SpatialIndex.partition"
