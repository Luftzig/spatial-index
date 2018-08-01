module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, float, list, string)
import Test exposing (..)

import List.Extra exposing (minimumBy, maximumBy)

import Spatial2DIndexing as SI exposing (BoundingBox, box, equal, getMinX, getMaxX, getMinY, getMaxY, compareMinX, compareMinXY)


fuzzBox = Fuzz.map4 box float float float float

fuzzBoxes = list fuzzBox

fuzzIndex =
    let
        bounded = Fuzz.map (List.indexedMap (\index box -> SI.boxValue box index)) fuzzBoxes
    in
        Fuzz.map (\list -> List.foldl (\bv rc -> SI.insert bv rc) SI.empty list) bounded


boxSuite : Test
boxSuite =
    describe "Checking box related logic"
        [fuzz fuzzBox "a box should be equal to itself" <|
            \box ->
                box
                    |> equal box
                    |> Expect.true "Expected box to equal to itself"
        , fuzz3 fuzzBox float float "Check ordering by minimal X" <|
            \b x1 x2 ->
                let
                    box1 = box (min x1 x2) (getMaxX b) (getMinY b) (getMaxY b)
                    box2 = box (max x1 x2) (getMaxX b) (getMinY b) (getMaxY b)
                in
                    compareMinX box1 box2
                        |> Expect.equal (compare (getMinX box1) (getMinX box2))
        , fuzz fuzzBox "minX <= maxX" <|
            \box ->
                Expect.atLeast (getMinX box) (getMaxX box)
        , fuzz fuzzBoxes "insertion preserves values" <|
            \boxes ->
                let
                    bounded = List.indexedMap (\index box -> SI.boxValue box index) boxes
                    rc = List.foldl (\bv rc -> SI.insert bv rc) SI.empty bounded
                in
                    Expect.equalLists (List.range 0 ((List.length bounded) - 1)) (SI.values rc |> List.sort)
        , fuzz fuzzBoxes "insertions preserves bounding boxes" <|
            \boxes ->
                let
                    bounded = List.indexedMap (\index box -> SI.boxValue box index) boxes
                    rc = List.foldl (\bv rc -> SI.insert bv rc) SI.empty bounded
                    coords box = (getMinX box, getMaxX box, getMinY box, getMaxY box)
                in
                    Expect.equalLists
                        (List.map SI.getBoundingBox bounded |> List.sortBy coords) (SI.boundingBoxes rc |> List.sortBy coords)
        , fuzz fuzzIndex "span of maximal range is everything" <|
            \index ->
                let
                    boxes = SI.boundingBoxes index
                    (minX, maxX, minY, maxY) =
                        ( minimumBy getMinX boxes |> Maybe.map getMinX |> Maybe.withDefault -10000
                        , maximumBy getMaxX boxes |> Maybe.map getMaxX |> Maybe.withDefault 10000
                        , minimumBy getMinY boxes |> Maybe.map getMinY |> Maybe.withDefault -10000
                        , maximumBy getMaxY boxes |> Maybe.map getMaxY |> Maybe.withDefault 10000
                        )
                    maxSpan = box minX maxX minY maxY
                in
                    Expect.equalLists (SI.span maxSpan index |> SI.values) (SI.values index)
        , fuzz fuzzIndex "span out of range is empty" <|
            \index ->
                let
                    boxes = SI.boundingBoxes index
                    (minX, maxX, minY, maxY) =
                        ( minimumBy getMinX boxes |> Maybe.map getMinX |> Maybe.withDefault -10000
                        , maximumBy getMaxX boxes |> Maybe.map getMaxX |> Maybe.withDefault 10000
                        , minimumBy getMinY boxes |> Maybe.map getMinY |> Maybe.withDefault -10000
                        , maximumBy getMaxY boxes |> Maybe.map getMaxY |> Maybe.withDefault 10000
                        )
                    maxSpan = box (minX - 2) (minX - 1) (minY - 2) (minY - 1)
                in
                    Expect.equal (0) (SI.span maxSpan index |> SI.values |> List.length)
        , fuzz fuzzIndex "span X out of range is empty" <|
            \index ->
                let
                    boxes = SI.boundingBoxes index
                    (minX, maxX, minY, maxY) =
                        ( minimumBy getMinX boxes |> Maybe.map getMinX |> Maybe.withDefault -10000
                        , maximumBy getMaxX boxes |> Maybe.map getMaxX |> Maybe.withDefault 10000
                        , minimumBy getMinY boxes |> Maybe.map getMinY |> Maybe.withDefault -10000
                        , maximumBy getMaxY boxes |> Maybe.map getMaxY |> Maybe.withDefault 10000
                        )
                    maxSpan = box (minX - 2) (minX - 1) minY maxY
                in
                    Expect.equal (0) (SI.span maxSpan index |> SI.values |> List.length)
        , fuzz fuzzIndex "span Y out of range is empty" <|
            \index ->
                let
                    boxes = SI.boundingBoxes index
                    (minX, maxX, minY, maxY) =
                        ( minimumBy getMinX boxes |> Maybe.map getMinX |> Maybe.withDefault -10000
                        , maximumBy getMaxX boxes |> Maybe.map getMaxX |> Maybe.withDefault 10000
                        , minimumBy getMinY boxes |> Maybe.map getMinY |> Maybe.withDefault -10000
                        , maximumBy getMaxY boxes |> Maybe.map getMaxY |> Maybe.withDefault 10000
                        )
                    maxSpan = box minX maxX (minY - 2) (minY - 1)
                in
                    Expect.equal (0) (SI.span maxSpan index |> SI.values |> List.length)
        , fuzz fuzzIndex "inner span is also maximal" <|
            \index ->
                let
                    boxes = SI.boundingBoxes index
                    (minX, maxX, minY, maxY) =
                        ( minimumBy getMaxX boxes |> Maybe.map getMaxX |> Maybe.withDefault -10000
                        , maximumBy getMinX boxes |> Maybe.map getMinX |> Maybe.withDefault 10000
                        , minimumBy getMaxY boxes |> Maybe.map getMaxY |> Maybe.withDefault -10000
                        , maximumBy getMinY boxes |> Maybe.map getMinY |> Maybe.withDefault 10000
                        )
                    maxSpan = box minX maxX minY maxY
                in
                    Expect.equalLists (SI.span maxSpan index |> SI.values) (SI.values index)
        ]
