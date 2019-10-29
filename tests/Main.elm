module Main exposing (delta, fuzzBox, fuzzBoxes, fuzzIndex, insertOperation)

import BoundingBox2d as BoundingBox exposing (fromExtrema, maxX, maxY, minX, minY)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Length exposing (Length, Meters)
import Quantity exposing (Quantity)
import SpatialIndex.SpatialIndex2D as SpatialIndex exposing (..)
import Test exposing (..)


delta : Float
delta =
    1.0e-6


fuzzLength : Fuzzer Length
fuzzLength =
    Fuzz.float |> Fuzz.map Length.meters


fuzzBox : Fuzzer (BoundingBox.BoundingBox2d Meters c)
fuzzBox =
    Fuzz.map4
        (\minX maxX minY maxY -> fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY })
        fuzzLength
        fuzzLength
        fuzzLength
        fuzzLength


fuzzBoxes : Fuzzer (List (BoundingBox.BoundingBox2d Meters c))
fuzzBoxes =
    Fuzz.list fuzzBox


type WorldCoordinates
    = WorldCoordinates Never


fuzzIndex : Fuzzer (SpatialIndex Meters WorldCoordinates Int)
fuzzIndex =
    let
        bounded =
            Fuzz.map (List.indexedMap (\index box -> SpatialIndex.element box index)) fuzzBoxes
    in
    Fuzz.map (\list -> List.foldl (\bv rc -> SpatialIndex.insert bv rc) SpatialIndex.empty list) bounded


insertOperation : Test
insertOperation =
    describe "Insert operation"
        [ fuzz fuzzBoxes "insertion preserves values" <|
            \boxes ->
                let
                    bounded =
                        List.indexedMap (\index box -> SpatialIndex.element box index) boxes

                    indexUnderTest =
                        List.foldl (\bv rc -> SpatialIndex.insert bv rc) SpatialIndex.empty bounded
                in
                Expect.equalLists (List.range 0 (List.length bounded - 1)) (SpatialIndex.values indexUnderTest |> List.sort)
        , fuzz fuzzBoxes "insertions preserves bounding boxes" <|
            \boxes ->
                let
                    bounded =
                        List.indexedMap (\idx box -> SpatialIndex.element box idx) boxes

                    index =
                        List.foldl (\bv rc -> SpatialIndex.insert bv rc) SpatialIndex.empty bounded

                    coords box =
                        ( ( getRawQuantity <| minX box
                          , getRawQuantity <| maxX box
                          )
                        , ( getRawQuantity <| minY box
                          , getRawQuantity <| maxY box
                          )
                        )
                in
                Expect.equalLists
                    (List.map SpatialIndex.bounds bounded |> List.sortBy coords)
                    (SpatialIndex.boundingBoxes index |> List.sortBy coords)
        ]


getRawQuantity : Quantity number unit -> number
getRawQuantity (Quantity.Quantity n) =
    n


minimumLength =
    Length.meters -1000000


maximumLength =
    Length.meters 1000000


spanSuite : Test
spanSuite =
    describe "Span operation"
        [ fuzz fuzzIndex "span of maximal range is everything" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.minX |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxX = boxes |> List.map BoundingBox.maxX |> Quantity.maximum |> Maybe.withDefault maximumLength
                        , minY = boxes |> List.map BoundingBox.minY |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxY = boxes |> List.map BoundingBox.maxY |> Quantity.maximum |> Maybe.withDefault maximumLength
                        }

                    maxSpan =
                        fromExtrema extrema
                in
                Expect.equalLists (SpatialIndex.span maxSpan index |> SpatialIndex.values) (SpatialIndex.values index)
        , fuzz fuzzIndex "span out of range is empty" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.minX |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxX = boxes |> List.map BoundingBox.maxX |> Quantity.maximum |> Maybe.withDefault maximumLength
                        , minY = boxes |> List.map BoundingBox.minY |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxY = boxes |> List.map BoundingBox.maxY |> Quantity.maximum |> Maybe.withDefault maximumLength
                        }

                    maxSpan =
                        fromExtrema
                            { minX = extrema.minX |> Quantity.minus (Length.meters 2)
                            , maxX = extrema.minX |> Quantity.minus (Length.meters 1)
                            , minY = extrema.minY |> Quantity.minus (Length.meters 2)
                            , maxY = extrema.minY |> Quantity.minus (Length.meters 1)
                            }
                in
                Expect.equal 0 (SpatialIndex.span maxSpan index |> SpatialIndex.values |> List.length)
        , fuzz fuzzIndex "span X out of range is empty" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.minX |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxX = boxes |> List.map BoundingBox.maxX |> Quantity.maximum |> Maybe.withDefault maximumLength
                        , minY = boxes |> List.map BoundingBox.minY |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxY = boxes |> List.map BoundingBox.maxY |> Quantity.maximum |> Maybe.withDefault maximumLength
                        }

                    maxSpan =
                        fromExtrema
                            { minX = extrema.minX |> Quantity.minus (Length.meters 2)
                            , maxX = extrema.minX |> Quantity.minus (Length.meters 1)
                            , minY = extrema.minY
                            , maxY = extrema.maxY
                            }
                in
                Expect.equal 0 (SpatialIndex.span maxSpan index |> SpatialIndex.values |> List.length)
        , fuzz fuzzIndex "span Y out of range is empty" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.minX |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxX = boxes |> List.map BoundingBox.maxX |> Quantity.maximum |> Maybe.withDefault maximumLength
                        , minY = boxes |> List.map BoundingBox.minY |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxY = boxes |> List.map BoundingBox.maxY |> Quantity.maximum |> Maybe.withDefault maximumLength
                        }

                    maxSpan =
                        fromExtrema
                            { minX = extrema.minX
                            , maxX = extrema.maxX
                            , minY = extrema.minY |> Quantity.minus (Length.meters 2)
                            , maxY = extrema.minY |> Quantity.minus (Length.meters 1)
                            }
                in
                Expect.equal 0 (SpatialIndex.span maxSpan index |> SpatialIndex.values |> List.length)
        , fuzz fuzzIndex "inner span is also maximal" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.maxX |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxX = boxes |> List.map BoundingBox.minX |> Quantity.maximum |> Maybe.withDefault maximumLength
                        , minY = boxes |> List.map BoundingBox.maxY |> Quantity.minimum |> Maybe.withDefault minimumLength
                        , maxY = boxes |> List.map BoundingBox.minY |> Quantity.maximum |> Maybe.withDefault maximumLength
                        }

                    maxSpan =
                        fromExtrema { minX = extrema.minX, maxX = extrema.maxX, minY = extrema.minY, maxY = extrema.maxY }
                in
                Expect.equalLists (SpatialIndex.span maxSpan index |> SpatialIndex.values) (SpatialIndex.values index)
        ]



--containedInSuite : Test
--containedInSuite =
--    describe "Similar to span, but requires that all elements are strictly contained in the range"
--        [ fuzz fuzzIndex "a range touching all elements is the maximal span" <|
--            \index ->
--                let
--                    boxes =
--                        SI.boundingBoxes index
--
--                    ( minX, maxX, minY, maxY ) =
--                        ( minimumBy BB.maxX boxes |> Maybe.map BB.maxX |> Maybe.withDefault -10000
--                        , maximumBy BB.minX boxes |> Maybe.map BB.minX |> Maybe.withDefault 10000
--                        , minimumBy BB.maxY boxes |> Maybe.map BB.maxY |> Maybe.withDefault -10000
--                        , maximumBy BB.minY boxes |> Maybe.map BB.minY |> Maybe.withDefault 10000
--                        )
--
--                    innerSpan =
--                        fromExtrema { minX = minX, maxX = maxX, minY = minY, maxY = maxY }
--                in
--                Expect.equalLists (SI.containedIn innerSpan index |> SI.values) (SI.values index)
--        , fuzz4 float float float float "preset values are contained in an index" <|
--            \x1 x2 y1 y2 ->
--                let
--                    minX_ =
--                        min x1 x2
--
--                    maxX_ =
--                        max x1 x2
--
--                    minY_ =
--                        min y1 y2
--
--                    maxY_ =
--                        max y1 y2
--
--                    range =
--                        fromExtrema { minX = minX_, maxX = maxX_, minY = minY_, maxY = maxY_ }
--
--                    contained =
--                        [ element (fromExtrema { minX = minX_, maxX = maxX_, minY = minY_, maxY = maxY_ }) "contained 1"
--                        , element (fromExtrema { minX = minX_ + delta, maxX = maxX_ - delta, minY = minY_ + delta, maxY = maxY_ - delta }) "contained 2"
--                        ]
--
--                    outside =
--                        [ element (fromExtrema { minX = minX_ - delta, maxX = maxX_, minY = minY_, maxY = maxY_ }) "out 1"
--                        , element (fromExtrema { minX = minX_, maxX = maxX_ + delta, minY = minY_, maxY = maxY_ }) "out 2"
--                        , element (fromExtrema { minX = minX_, maxX = maxX_, minY = minY_ - delta, maxY = maxY_ }) "out 3"
--                        , element (fromExtrema { minX = minX_, maxX = maxX_, minY = minY_, maxY = maxY_ + delta }) "out 4"
--                        ]
--
--                    index =
--                        SI.fromElements <| (contained |> List.append outside)
--                in
--                Expect.equalLists [ "contained 1", "contained 2" ] (containedIn range index |> SI.values |> List.sort)
--        ]
--
--
--partitionByLineSuite : Test
--partitionByLineSuite =
--    -- TODO: Need to check other directions and possibly not axis aligned directions, too
--    describe "Partition index by a line"
--        [ fuzz fuzzIndex "Elements space separated by medial X" <|
--            \index ->
--                let
--                    sorted =
--                        SI.elements index |> List.sortBy (bounds >> maxX)
--
--                    lower =
--                        List.take (List.length sorted // 2) sorted
--
--                    upper =
--                        List.drop (List.length sorted // 2) sorted
--
--                    medial =
--                        Maybe.map (bounds >> maxX) (List.head upper)
--
--                    point =
--                        Maybe.map (\x -> fromCoordinates ( x, 0 )) medial
--
--                    result =
--                        Maybe.map (\p -> partitionByLine p positiveX index) point
--
--                    lowerResult =
--                        Maybe.map Tuple.second
--
--                    upperResult =
--                        Maybe.map Tuple.first
--                in
--                Expect.all
--                    [ \res ->
--                        Expect.equalLists
--                            (List.map value lower |> List.sort)
--                            (lowerResult res
--                                |> Maybe.map values
--                                |> Maybe.withDefault []
--                            )
--                    , \res ->
--                        Expect.equalLists
--                            (List.map value upper |> List.sort)
--                            (upperResult res
--                                |> Maybe.map values
--                                |> Maybe.withDefault []
--                            )
--                    ]
--                    result
--        ]
--
--
--partitionByBoundsSuite : Test
--partitionByBoundsSuite =
--    describe "Partition index by a bounding box"
--        [ fuzz fuzzIndex "If bounding box does not intersect index then one set is empty" <|
--            \index ->
--                let
--                    minIndexX =
--                        SI.elements index |> List.map (bounds >> minX) |> List.minimum
--
--                    minIndexY =
--                        SI.elements index |> List.map (bounds >> minY) |> List.minimum
--
--                    externalBounds =
--                        case ( minIndexX, minIndexY ) of
--                            ( Just indexX, Just indexY ) ->
--                                fromExtrema
--                                    { minX = indexX - 1
--                                    , minY = indexY - 1
--                                    , maxX = indexX
--                                    , maxY = indexY
--                                    }
--
--                            _ ->
--                                fromExtrema { minX = 0, minY = 0, maxX = 0, maxY = 0 }
--
--                    ( inGroup, outGroup ) =
--                        SI.partitionByBounds externalBounds index
--                in
--                Expect.all
--                    [ \_ -> Expect.equal 0 (List.length <| SI.elements inGroup)
--                    , \_ -> Expect.equal (List.length <| SI.elements outGroup) (List.length <| SI.elements index)
--                    ]
--                    ()
--        ]
--
--
--mergeSuite : Test
--mergeSuite =
--    describe "Merge two indices togather"
--        []
--
--
--collisionsSuite : Test
--collisionsSuite =
--    describe "Find all colliding pairs in the index"
--        []
--
--
--mapSuite : Test
--mapSuite =
--    describe "Map all elements and restructure the index if required"
--        []
--
--
--mapValuesSuite : Test
--mapValuesSuite =
--    describe "Map all values without changing their bounding boxes"
--        []
--
--
--nearestSuite : Test
--nearestSuite =
--    describe "Find N nearests elements to point"
--        []
--
--
--removeSuite : Test
--removeSuite =
--    describe "Remove everything that intersects with a bounding box"
--        []
