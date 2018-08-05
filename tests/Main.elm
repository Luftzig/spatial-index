module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, float, list, string)
import Test exposing (..)
import List.Extra exposing (minimumBy, maximumBy)
import SpatialIndex.SpatialIndex2D as SI exposing (..)
import BoundingBox2d as BB exposing (fromExtrema, maxX, maxY, minX, minY)


fuzzBox =
    Fuzz.map4 (\minX maxX minY maxY -> fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }) float float float float


fuzzBoxes =
    list fuzzBox


fuzzIndex =
    let
        bounded =
            Fuzz.map (List.indexedMap (\index box -> SI.boxValue box index)) fuzzBoxes
    in
        Fuzz.map (\list -> List.foldl (\bv rc -> SI.insert bv rc) SI.empty list) bounded


insertOperation : Test
insertOperation =
    describe "Insert operation"
        [ fuzz fuzzBoxes "insertion preserves values" <|
            \boxes ->
                let
                    bounded =
                        List.indexedMap (\index box -> SI.boxValue box index) boxes

                    rc =
                        List.foldl (\bv rc -> SI.insert bv rc) SI.empty bounded
                in
                    Expect.equalLists (List.range 0 ((List.length bounded) - 1)) (SI.values rc |> List.sort)
        , fuzz fuzzBoxes "insertions preserves bounding boxes" <|
            \boxes ->
                let
                    bounded =
                        List.indexedMap (\index box -> SI.boxValue box index) boxes

                    rc =
                        List.foldl (\bv rc -> SI.insert bv rc) SI.empty bounded

                    coords box =
                        ( minY box, maxX box, minY box, maxY box )
                in
                    Expect.equalLists
                        (List.map SI.getBoundingBox bounded |> List.sortBy coords)
                        (SI.boundingBoxes rc |> List.sortBy coords)
        ]


spanSuite : Test
spanSuite =
    describe "Span operation"
        [ fuzz fuzzIndex "span of maximal range is everything" <|
            \index ->
                let
                    boxes =
                        SI.boundingBoxes index

                    ( minX, maxX, minY, maxY ) =
                        ( minimumBy BB.minX boxes |> Maybe.map BB.minX |> Maybe.withDefault -10000
                        , maximumBy BB.maxX boxes |> Maybe.map BB.maxX |> Maybe.withDefault 10000
                        , minimumBy BB.minY boxes |> Maybe.map BB.minY |> Maybe.withDefault -10000
                        , maximumBy BB.maxY boxes |> Maybe.map BB.maxY |> Maybe.withDefault 10000
                        )

                    maxSpan =
                        fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }
                in
                    Expect.equalLists (SI.span maxSpan index |> SI.values) (SI.values index)
        , fuzz fuzzIndex "span out of range is empty" <|
            \index ->
                let
                    boxes =
                        SI.boundingBoxes index

                    ( minX, maxX, minY, maxY ) =
                        ( minimumBy BB.minX boxes |> Maybe.map BB.minX |> Maybe.withDefault -10000
                        , maximumBy BB.maxX boxes |> Maybe.map BB.maxX |> Maybe.withDefault 10000
                        , minimumBy BB.minY boxes |> Maybe.map BB.minY |> Maybe.withDefault -10000
                        , maximumBy BB.maxY boxes |> Maybe.map BB.maxY |> Maybe.withDefault 10000
                        )

                    maxSpan =
                        fromExtrema { minX = (minX - 2), maxX = (minX - 1), minY = (minY - 2), maxY = (minY - 1) }
                in
                    Expect.equal (0) (SI.span maxSpan index |> SI.values |> List.length)
        , fuzz fuzzIndex "span X out of range is empty" <|
            \index ->
                let
                    boxes =
                        SI.boundingBoxes index

                    ( minX, maxX, minY, maxY ) =
                        ( minimumBy BB.minX boxes |> Maybe.map BB.minX |> Maybe.withDefault -10000
                        , maximumBy BB.maxX boxes |> Maybe.map BB.maxX |> Maybe.withDefault 10000
                        , minimumBy BB.minY boxes |> Maybe.map BB.minY |> Maybe.withDefault -10000
                        , maximumBy BB.maxY boxes |> Maybe.map BB.maxY |> Maybe.withDefault 10000
                        )

                    maxSpan =
                        fromExtrema { minX = (minX - 2), maxX = (minX - 1), minY = minY, maxY = maxY }
                in
                    Expect.equal (0) (SI.span maxSpan index |> SI.values |> List.length)
        , fuzz fuzzIndex "span Y out of range is empty" <|
            \index ->
                let
                    boxes =
                        SI.boundingBoxes index

                    ( minX, maxX, minY, maxY ) =
                        ( minimumBy BB.minX boxes |> Maybe.map BB.minX |> Maybe.withDefault -10000
                        , maximumBy BB.maxX boxes |> Maybe.map BB.maxX |> Maybe.withDefault 10000
                        , minimumBy BB.minY boxes |> Maybe.map BB.minY |> Maybe.withDefault -10000
                        , maximumBy BB.maxY boxes |> Maybe.map BB.maxY |> Maybe.withDefault 10000
                        )

                    maxSpan =
                        fromExtrema { minX = minX, maxX = maxX, minY = (minY - 2), maxY = (minY - 1) }
                in
                    Expect.equal (0) (SI.span maxSpan index |> SI.values |> List.length)
        , fuzz fuzzIndex "inner span is also maximal" <|
            \index ->
                let
                    boxes =
                        SI.boundingBoxes index

                    ( minX, maxX, minY, maxY ) =
                        ( minimumBy BB.maxX boxes |> Maybe.map BB.maxX |> Maybe.withDefault -10000
                        , maximumBy BB.minX boxes |> Maybe.map BB.minX |> Maybe.withDefault 10000
                        , minimumBy BB.maxY boxes |> Maybe.map BB.maxY |> Maybe.withDefault -10000
                        , maximumBy BB.minY boxes |> Maybe.map BB.minY |> Maybe.withDefault 10000
                        )

                    maxSpan =
                        fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }
                in
                    Expect.equalLists (SI.span maxSpan index |> SI.values) (SI.values index)
        ]
