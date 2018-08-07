module Main exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, float, list, string)
import Test exposing (..)
import List.Extra exposing (minimumBy, maximumBy)
import SpatialIndex.SpatialIndex2D as SI exposing (..)
import BoundingBox2d as BB exposing (fromExtrema, maxX, maxY, minX, minY)
import Point2d exposing (fromCoordinates)
import Direction2d exposing (positiveX)


delta =
    1.0e-6


fuzzBox =
    Fuzz.map4 (\minX maxX minY maxY -> fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }) float float float float


fuzzBoxes =
    list fuzzBox


fuzzIndex =
    let
        bounded =
            Fuzz.map (List.indexedMap (\index box -> SI.element box index)) fuzzBoxes
    in
        Fuzz.map (\list -> List.foldl (\bv rc -> SI.insert bv rc) SI.empty list) bounded


insertOperation : Test
insertOperation =
    describe "Insert operation"
        [ fuzz fuzzBoxes "insertion preserves values" <|
            \boxes ->
                let
                    bounded =
                        List.indexedMap (\index box -> SI.element box index) boxes

                    rc =
                        List.foldl (\bv rc -> SI.insert bv rc) SI.empty bounded
                in
                    Expect.equalLists (List.range 0 ((List.length bounded) - 1)) (SI.values rc |> List.sort)
        , fuzz fuzzBoxes "insertions preserves bounding boxes" <|
            \boxes ->
                let
                    bounded =
                        List.indexedMap (\index box -> SI.element box index) boxes

                    index =
                        List.foldl (\bv rc -> SI.insert bv rc) SI.empty bounded

                    coords box =
                        ( minY box, maxX box, minY box, maxY box )
                in
                    Expect.equalLists
                        (List.map SI.bounds bounded |> List.sortBy coords)
                        (SI.boundingBoxes index |> List.sortBy coords)
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


containedInSuite : Test
containedInSuite =
    describe "Similar to span, but requires that all elements are strictly contained in the range"
        [ fuzz fuzzIndex "a range touching all elements is the maximal span" <|
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

                    innerSpan =
                        fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }
                in
                    Expect.equalLists (SI.containedIn innerSpan index |> SI.values) (SI.values index)
        , fuzz4 float float float float "preset values are contained in an index" <|
            \x1 x2 y1 y2 ->
                let
                    minX_ =
                        min x1 x2

                    maxX_ =
                        max x1 x2

                    minY_ =
                        min y1 y2

                    maxY_ =
                        max y1 y2

                    range =
                        fromExtrema { minX = minX_, maxX = maxX_, minY = minY_, maxY = maxY_ }

                    contained =
                        [ element (fromExtrema { minX = minX_, maxX = maxX_, minY = minY_, maxY = maxY_ }) "contained 1"
                        , element (fromExtrema { minX = minX_ + delta, maxX = maxX_ - delta, minY = minY_ + delta, maxY = maxY_ - delta }) "contained 2"
                        ]

                    outside =
                        [ element (fromExtrema { minX = minX_ - delta, maxX = maxX_, minY = minY_, maxY = maxY_ }) "out 1"
                        , element (fromExtrema { minX = minX_, maxX = maxX_ + delta, minY = minY_, maxY = maxY_ }) "out 2"
                        , element (fromExtrema { minX = minX_, maxX = maxX_, minY = minY_ - delta, maxY = maxY_ }) "out 3"
                        , element (fromExtrema { minX = minX_, maxX = maxX_, minY = minY_, maxY = maxY_ + delta }) "out 4"
                        ]

                    index =
                        SI.fromElements <| (contained |> List.append outside)
                in
                    Expect.equalLists [ "contained 1", "contained 2" ] (containedIn range index |> SI.values |> List.sort)
        ]


partitionByLineSuite : Test
partitionByLineSuite =
    -- TODO: Need to check other directions and possibly not axis aligned directions, too
    describe "Partition index by a line"
        [ fuzz fuzzIndex "Elements space separated by medial X" <|
            \index ->
                let
                    sorted =
                        SI.elements index |> List.sortBy (bounds >> maxX)

                    lower =
                        List.take (List.length sorted // 2) sorted

                    upper =
                        List.drop (List.length sorted // 2) sorted

                    medial =
                        Maybe.map (bounds >> maxX) (List.head upper)

                    point =
                        Maybe.map (\x -> fromCoordinates ( x, 0 )) medial

                    result =
                        Maybe.map (\p -> partitionByLine p positiveX index) point

                    lowerResult =
                        Maybe.map Tuple.second

                    upperResult =
                        Maybe.map Tuple.first
                in
                    Expect.all
                        [ \res ->
                            Expect.equalLists
                                (List.map value lower |> List.sort)
                                (lowerResult res
                                    |> Maybe.map values
                                    |> Maybe.withDefault []
                                )
                        , \res ->
                            Expect.equalLists
                                (List.map value upper |> List.sort)
                                (upperResult res
                                    |> Maybe.map values
                                    |> Maybe.withDefault []
                                )
                        ]
                        result
        ]


partitionByBoundsSuite : Test
partitionByBoundsSuite =
    describe "Partition index by a bounding box"
        []


mergeSuite : Test
mergeSuite =
    describe "Merge two indices togather"
        []


collisionsSuite : Test
collisionsSuite =
    describe "Find all colliding pairs in the index"
        []


mapSuite : Test
mapSuite =
    describe "Map all elements and restructure the index if required"
        []


mapValuesSuite : Test
mapValuesSuite =
    describe "Map all values without changing their bounding boxes"
        []


nearestSuite : Test
nearestSuite =
    describe "Find N nearests elements to point"
        []


removeSuite : Test
removeSuite =
    describe "Remove everything that intersects with a bounding box"
        []
