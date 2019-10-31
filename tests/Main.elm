module Main exposing (containedInSuite, insertOperation, intersectsSuite, partitionByLineSuite)

import BoundingBox2d as BoundingBox exposing (fromExtrema, maxX, maxY, minX, minY)
import Direction2d exposing (positiveX)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Length exposing (Length, Meters)
import Point2d
import Quantity exposing (Quantity)
import Set
import SpatialIndex.SpatialIndex2D as SpatialIndex exposing (..)
import Test exposing (..)


delta : Float
delta =
    1.0e-6


lengthDelta : Length
lengthDelta =
    Length.meters delta


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


lengthLowerBound =
    Length.meters -1000000


lengthUpperBound =
    Length.meters 1000000


minimumBy : (a -> Quantity number u) -> List a -> Maybe (Quantity number u)
minimumBy f xs =
    xs |> List.map f |> Quantity.minimum


maximumBy : (a -> Quantity number u) -> List a -> Maybe (Quantity number u)
maximumBy f xs =
    xs |> List.map f |> Quantity.maximum


equalAsSets : List comparable -> List comparable -> Expectation
equalAsSets xs ys =
    Set.fromList xs |> Expect.equalSets (Set.fromList ys)


equalAsSetsBy : (a -> comparable) -> List a -> List a -> Expectation
equalAsSetsBy f xs ys =
    equalAsSets (List.map f xs) (List.map f ys)


equalIndexValues : SpatialIndex q c comparable -> SpatialIndex q c comparable -> Expectation
equalIndexValues index1 index2 =
    SpatialIndex.values index1 |> equalAsSets (SpatialIndex.values index2)


intersectsSuite : Test
intersectsSuite =
    describe "Intersect operation"
        [ fuzz fuzzIndex "The maximal size of index intersects with all elements" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = minimumBy BoundingBox.minX boxes |> Maybe.withDefault lengthLowerBound
                        , maxX = maximumBy BoundingBox.maxX boxes |> Maybe.withDefault lengthUpperBound
                        , minY = minimumBy BoundingBox.minY boxes |> Maybe.withDefault lengthLowerBound
                        , maxY = maximumBy BoundingBox.maxY boxes |> Maybe.withDefault lengthUpperBound
                        }

                    maxSpan =
                        fromExtrema extrema
                in
                SpatialIndex.intersectingWith maxSpan index |> equalIndexValues index
        , fuzz fuzzIndex "intersect out of range is empty" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.minX |> Quantity.minimum |> Maybe.withDefault lengthLowerBound
                        , maxX = boxes |> List.map BoundingBox.maxX |> Quantity.maximum |> Maybe.withDefault lengthUpperBound
                        , minY = boxes |> List.map BoundingBox.minY |> Quantity.minimum |> Maybe.withDefault lengthLowerBound
                        , maxY = boxes |> List.map BoundingBox.maxY |> Quantity.maximum |> Maybe.withDefault lengthUpperBound
                        }

                    maxSpan =
                        fromExtrema
                            { minX = extrema.minX |> Quantity.minus (Length.meters 2)
                            , maxX = extrema.minX |> Quantity.minus (Length.meters 1)
                            , minY = extrema.minY |> Quantity.minus (Length.meters 2)
                            , maxY = extrema.minY |> Quantity.minus (Length.meters 1)
                            }
                in
                Expect.equal 0 (SpatialIndex.intersectingWith maxSpan index |> SpatialIndex.values |> List.length)
        , fuzz fuzzIndex "intersect X out of range is empty" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.minX |> Quantity.minimum |> Maybe.withDefault lengthLowerBound
                        , maxX = boxes |> List.map BoundingBox.maxX |> Quantity.maximum |> Maybe.withDefault lengthUpperBound
                        , minY = boxes |> List.map BoundingBox.minY |> Quantity.minimum |> Maybe.withDefault lengthLowerBound
                        , maxY = boxes |> List.map BoundingBox.maxY |> Quantity.maximum |> Maybe.withDefault lengthUpperBound
                        }

                    maxSpan =
                        fromExtrema
                            { minX = extrema.minX |> Quantity.minus (Length.meters 2)
                            , maxX = extrema.minX |> Quantity.minus (Length.meters 1)
                            , minY = extrema.minY
                            , maxY = extrema.maxY
                            }
                in
                Expect.equal 0 (SpatialIndex.intersectingWith maxSpan index |> SpatialIndex.values |> List.length)
        , fuzz fuzzIndex "intersect Y out of range is empty" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.minX |> Quantity.minimum |> Maybe.withDefault lengthLowerBound
                        , maxX = boxes |> List.map BoundingBox.maxX |> Quantity.maximum |> Maybe.withDefault lengthUpperBound
                        , minY = boxes |> List.map BoundingBox.minY |> Quantity.minimum |> Maybe.withDefault lengthLowerBound
                        , maxY = boxes |> List.map BoundingBox.maxY |> Quantity.maximum |> Maybe.withDefault lengthUpperBound
                        }

                    maxSpan =
                        fromExtrema
                            { minX = extrema.minX
                            , maxX = extrema.maxX
                            , minY = extrema.minY |> Quantity.minus (Length.meters 2)
                            , maxY = extrema.minY |> Quantity.minus (Length.meters 1)
                            }
                in
                Expect.equal 0 (SpatialIndex.intersectingWith maxSpan index |> SpatialIndex.values |> List.length)
        , fuzz fuzzIndex "inner intersect is also maximal" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    extrema =
                        { minX = boxes |> List.map BoundingBox.maxX |> Quantity.minimum |> Maybe.withDefault lengthLowerBound
                        , maxX = boxes |> List.map BoundingBox.minX |> Quantity.maximum |> Maybe.withDefault lengthUpperBound
                        , minY = boxes |> List.map BoundingBox.maxY |> Quantity.minimum |> Maybe.withDefault lengthLowerBound
                        , maxY = boxes |> List.map BoundingBox.minY |> Quantity.maximum |> Maybe.withDefault lengthUpperBound
                        }

                    maxSpan =
                        fromExtrema { minX = extrema.minX, maxX = extrema.maxX, minY = extrema.minY, maxY = extrema.maxY }
                in
                (SpatialIndex.intersectingWith maxSpan index |> SpatialIndex.values) |> equalAsSets (SpatialIndex.values index)
        ]


containedInSuite : Test
containedInSuite =
    describe "Retrieve all elements of an index strictly contained in a given range"
        [ fuzz fuzzIndex "a range touching all elements is the maximal span" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    innerSpan =
                        fromExtrema
                            { minX = minimumBy BoundingBox.maxX boxes |> Maybe.withDefault lengthLowerBound
                            , maxX = maximumBy BoundingBox.minX boxes |> Maybe.withDefault lengthUpperBound
                            , minY = minimumBy BoundingBox.maxY boxes |> Maybe.withDefault lengthLowerBound
                            , maxY = maximumBy BoundingBox.minY boxes |> Maybe.withDefault lengthUpperBound
                            }
                in
                SpatialIndex.containedIn innerSpan index |> equalIndexValues index
        , fuzz fuzzBox "elements created to match query are separated correctly" <|
            \range ->
                let
                    minX_ =
                        BoundingBox.minX range

                    maxX_ =
                        BoundingBox.minX range

                    minY_ =
                        BoundingBox.minY range

                    maxY_ =
                        BoundingBox.maxY range

                    contained =
                        [ element (fromExtrema { minX = minX_, maxX = maxX_, minY = minY_, maxY = maxY_ }) "contained 1"
                        , element
                            (fromExtrema
                                { minX = minX_ |> Quantity.plus lengthDelta
                                , maxX = maxX_ |> Quantity.minus lengthDelta
                                , minY = minY_ |> Quantity.plus lengthDelta
                                , maxY = maxY_ |> Quantity.minus lengthDelta
                                }
                            )
                            "contained 2"
                        ]

                    outside =
                        [ element
                            (fromExtrema
                                { minX = minX_ |> Quantity.minus lengthDelta
                                , maxX = maxX_
                                , minY = minY_
                                , maxY = maxY_
                                }
                            )
                            "out 1"
                        , element
                            (fromExtrema
                                { minX = minX_
                                , maxX = maxX_ |> Quantity.plus lengthDelta
                                , minY = minY_
                                , maxY = maxY_
                                }
                            )
                            "out 2"
                        , element
                            (fromExtrema
                                { minX = minX_
                                , maxX = maxX_
                                , minY = minY_ |> Quantity.minus lengthDelta
                                , maxY = maxY_
                                }
                            )
                            "out 3"
                        , element
                            (fromExtrema
                                { minX = minX_
                                , maxX = maxX_
                                , minY = minY_
                                , maxY = maxY_ |> Quantity.plus lengthDelta
                                }
                            )
                            "out 4"
                        ]

                    index =
                        SpatialIndex.fromElements <| (contained |> List.append outside)
                in
                [ "contained 1", "contained 2" ]
                    |> equalAsSets (containedIn range index |> SpatialIndex.values)
        ]


partitionByLineSuite : Test
partitionByLineSuite =
    describe "Partition index by a line"
        [ fuzz2 fuzzIndex Fuzz.float "The size of both results is the same as the input" <|
            \index xCoordinate ->
                let
                    ( left, right ) =
                        SpatialIndex.partitionByLine
                            (Point2d.fromMeters { x = xCoordinate, y = 0 })
                            Direction2d.positiveY
                            index
                in
                SpatialIndex.size index |> Expect.equal (SpatialIndex.size left + SpatialIndex.size right)
        ]



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
