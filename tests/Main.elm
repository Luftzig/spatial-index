module Main exposing (collisionsSuite, containedInSuite, insertOperation, intersectsSuite, mergeSuite, partitionIntersectingSuit, spanSuite)

import BoundingBox2d as BoundingBox exposing (fromExtrema, maxX, maxY, minX, minY)
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Length exposing (Length, Meters)
import Point2d
import Quantity exposing (Quantity)
import Set
import SpatialIndex.SpatialIndex2D as SpatialIndex exposing (..)
import Test exposing (..)


epsilon : Float
epsilon =
    2 ^ -52


epsilonLength : Length
epsilonLength =
    Length.meters epsilon


plusScaledEpsilon : Quantity Float Meters -> Quantity Float Meters
plusScaledEpsilon v =
    Quantity.max (v |> Quantity.plus epsilonLength) (v |> Quantity.multiplyBy (1 + epsilon))


minusScaledEpsilon : Quantity Float Meters -> Quantity Float Meters
minusScaledEpsilon v =
    Quantity.max (v |> Quantity.minus epsilonLength) (v |> Quantity.multiplyBy (1 - epsilon))


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
    describe "SpatialIndex.insert"
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


equalListsWithoutOrder : List comparable -> List comparable -> Expectation
equalListsWithoutOrder xs ys =
    Set.fromList xs |> Expect.equalSets (Set.fromList ys)


equalAsSetsBy : (a -> comparable) -> List a -> List a -> Expectation
equalAsSetsBy f xs ys =
    equalListsWithoutOrder (List.map f xs) (List.map f ys)


equalIndexValues : SpatialIndex q c comparable -> SpatialIndex q c comparable -> Expectation
equalIndexValues index1 index2 =
    SpatialIndex.values index1 |> equalListsWithoutOrder (SpatialIndex.values index2)


spanSuite : Test
spanSuite =
    describe "SpatialIndex.span - the maximal bounding box of the index"
        [ fuzz fuzzIndex "the span contains all elements" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    { maybeMaxX, maybeMinX, maybeMaxY, maybeMinY } =
                        { maybeMaxX = maximumBy maxX boxes
                        , maybeMinX = minimumBy minX boxes
                        , maybeMaxY = maximumBy maxY boxes
                        , maybeMinY = minimumBy minY boxes
                        }

                    expected =
                        case ( maybeMaxX, maybeMinX ) of
                            ( Just maxX_, Just minX_ ) ->
                                case ( maybeMaxY, maybeMinY ) of
                                    ( Just maxY_, Just minY_ ) ->
                                        Just <| fromExtrema { maxX = maxX_, minX = minX_, maxY = maxY_, minY = minY_ }

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing
                in
                expected |> Expect.equal (SpatialIndex.span index)
        ]


intersectsSuite : Test
intersectsSuite =
    describe "SpatialIndex.intersect"
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
                (SpatialIndex.intersectingWith maxSpan index |> SpatialIndex.values) |> equalListsWithoutOrder (SpatialIndex.values index)
        ]


containedInSuite : Test
containedInSuite =
    describe "SpatialIndex.containedIn"
        [ fuzz fuzzIndex "a range touching all elements is the maximal span" <|
            \index ->
                let
                    boxes =
                        SpatialIndex.boundingBoxes index

                    innerSpan =
                        fromExtrema
                            { minX = minimumBy BoundingBox.minX boxes |> Maybe.withDefault lengthLowerBound
                            , maxX = maximumBy BoundingBox.maxX boxes |> Maybe.withDefault lengthUpperBound
                            , minY = minimumBy BoundingBox.minY boxes |> Maybe.withDefault lengthLowerBound
                            , maxY = maximumBy BoundingBox.maxY boxes |> Maybe.withDefault lengthUpperBound
                            }
                in
                SpatialIndex.containedIn innerSpan index |> equalIndexValues index
        , fuzz fuzzBox "elements created to exceed range are not returned" <|
            \range ->
                let
                    minX_ =
                        BoundingBox.minX range

                    maxX_ =
                        BoundingBox.maxX range

                    minY_ =
                        BoundingBox.minY range

                    maxY_ =
                        BoundingBox.maxY range

                    contained =
                        [ element range "contained 1"
                        ]

                    outside =
                        [ element
                            (fromExtrema
                                { minX = minX_ |> Quantity.minus (Length.meters 1)
                                , maxX = maxX_
                                , minY = minY_
                                , maxY = maxY_
                                }
                            )
                            "out 1"
                        , element
                            (fromExtrema
                                { minX = minX_
                                , maxX = maxX_ |> Quantity.plus (Length.meters 10)
                                , minY = minY_
                                , maxY = maxY_
                                }
                            )
                            "out 2"
                        , element
                            (fromExtrema
                                { minX = minX_
                                , maxX = maxX_
                                , minY = minY_ |> Quantity.minus (Length.meters 1)
                                , maxY = maxY_
                                }
                            )
                            "out 3"
                        , element
                            (fromExtrema
                                { minX = minX_
                                , maxX = maxX_
                                , minY = minY_
                                , maxY = maxY_ |> Quantity.plus (Length.meters 1)
                                }
                            )
                            "out 4"
                        ]

                    index =
                        SpatialIndex.fromElements <| (contained |> List.append outside)

                    resultIndex =
                        containedIn range index
                in
                [ "contained 1" ]
                    |> equalListsWithoutOrder (SpatialIndex.values resultIndex)
        ]


partitionIntersectingSuit : Test
partitionIntersectingSuit =
    describe "SpatialIndex.partitionIntersecting"
        [ fuzz2 fuzzIndex fuzzBox "the sum of partitions is equal the whole" <|
            \index range ->
                let
                    ( inSet, outSet ) =
                        SpatialIndex.partitionIntersecting range index
                in
                Expect.equal (size index) (size inSet + size outSet)
        , fuzz fuzzIndex "If bounding box does not intersect index then first set is empty" <|
            \index ->
                let
                    minIndexX =
                        SpatialIndex.elements index |> List.map (bounds >> minX) |> Quantity.minimum

                    minIndexY =
                        SpatialIndex.elements index |> List.map (bounds >> minY) |> Quantity.minimum

                    range =
                        case ( minIndexX, minIndexY ) of
                            ( Just indexX, Just indexY ) ->
                                BoundingBox.singleton <|
                                    Point2d.xy (indexX |> Quantity.minus (Length.meters 1))
                                        (indexY |> Quantity.minus (Length.meters 1))

                            _ ->
                                fromExtrema
                                    { minX = Quantity.zero
                                    , minY = Quantity.zero
                                    , maxX = Quantity.zero
                                    , maxY = Quantity.zero
                                    }

                    ( inGroup, outGroup ) =
                        SpatialIndex.partitionIntersecting range index
                in
                Expect.all
                    [ \_ -> Expect.equal (size inGroup) 0
                    , \_ -> Expect.equal (size outGroup) (size index)
                    ]
                    ()
        , fuzz fuzzIndex "If range is the span then the first set is equal index" <|
            \index ->
                let
                    range =
                        SpatialIndex.span index

                    ( inGroup, outGroup ) =
                        case range of
                            Just range_ ->
                                SpatialIndex.partitionIntersecting range_ index

                            Nothing ->
                                SpatialIndex.partitionIntersecting (BoundingBox.singleton Point2d.origin) index
                in
                Expect.all
                    [ \_ -> Expect.equal (size inGroup) (size index)
                    , \_ -> Expect.equal (size outGroup) 0
                    ]
                    ()
        ]


mergeSuite : Test
mergeSuite =
    describe "SpatialIndex.merge - create index with elements of both"
        [ fuzz2 fuzzIndex fuzzIndex "Size of merged index should equal sum sources" <|
            \index1 index2 ->
                Expect.equal (size index1 + size index2) (size <| SpatialIndex.merge index1 index2)
        , fuzz2 fuzzIndex fuzzIndex "Merged index should have values from both sources" <|
            \index1 index2 ->
                equalListsWithoutOrder (SpatialIndex.values index1 |> List.append (SpatialIndex.values index2))
                    (SpatialIndex.values <| SpatialIndex.merge index1 index2)
        ]


collisionsSuite : Test
collisionsSuite =
    describe "SpatialIndex.collisons"
        [ fuzz fuzzIndex "The number of collisions is ≤ N^2" <|
            \index ->
                (SpatialIndex.collisions index |> List.length) |> Expect.atMost (size index ^ 2)
        , fuzz fuzzIndex "An element the size of the whole index collides with all other elements" <|
            \index ->
                let
                    maxValue =
                        List.maximum <| SpatialIndex.values index

                    span =
                        SpatialIndex.span index

                    originalValues =
                        SpatialIndex.values index

                    maximalElement =
                        case ( maxValue, span ) of
                            ( Just maxVal, Just span_ ) ->
                                element span_ (maxVal + 1)

                            _ ->
                                element (BoundingBox.singleton Point2d.origin) 0

                    getNoneMaximal ( e1, e2 ) =
                        if e1 == maximalElement then
                            Just (value e2)

                        else if e2 == maximalElement then
                            Just (value e1)

                        else
                            Nothing

                    indexWithMaximal =
                        SpatialIndex.insert maximalElement index

                    collisionsList =
                        SpatialIndex.collisions indexWithMaximal
                in
                List.filterMap getNoneMaximal collisionsList |> equalListsWithoutOrder originalValues
        , fuzz fuzzIndex "A collision only appears once" <|
            \index ->
                let
                    collisionsList =
                        SpatialIndex.collisions index

                    collidedValues =
                        Set.fromList <| List.map (\( e1, e2 ) -> ( value e1, value e2 )) collisionsList

                    reversed =
                        Set.map (\( v1, v2 ) -> ( v2, v1 )) collidedValues

                    duplicatedValues =
                        Set.intersect collidedValues reversed
                in
                Set.size duplicatedValues |> Expect.equal 0
        ]



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
