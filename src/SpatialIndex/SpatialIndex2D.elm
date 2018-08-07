module SpatialIndex.SpatialIndex2D
    exposing
        ( SpatialIndex
        , BoxBoundedValue
        , bounds
        , value
        , element
        , empty
        , fromElements
        , maxBoundingBox
        , insert
        , values
        , boundingBoxes
        , elements
        , span
        , containedIn
        , partitionByLine
        , partitionByBounds
        , merge
        , collisions
        , map
        , mapValues
        , nearest
        , remove
        )

import Direction2d exposing (Direction2d)
import Point2d exposing (Point2d)
import Tuple exposing (first, second)
import List.Extra exposing (dropWhile, takeWhile)
import BoundingBox2d as BoundingBox exposing (BoundingBox2d, minX, minY, extrema, intersects)


{-| A Two dimensional spatial index
-}
type SpatialIndex a
    = SpatialIndex
        { xSorted : List (BoxBoundedValue a)
        , ySorted : List (BoxBoundedValue a)
        }


{-| An element of an index, made up from a value `a` and a `BoundingBox2d`
-}
type alias BoxBoundedValue a =
    ( BoundingBox2d, a )


{-| Bind a value `a` with a bounding box to create a valid index value.
-}
element : BoundingBox2d -> a -> BoxBoundedValue a
element box val =
    ( box, val )


{-| Extract the bounding box out of an index element
-}
bounds : BoxBoundedValue a -> BoundingBox2d
bounds ( box, value ) =
    box


{-| Extract the value out of an index element
-}
value : BoxBoundedValue a -> a
value ( box, value ) =
    value


{-| Construct an empty index
-}
empty : SpatialIndex a
empty =
    SpatialIndex
        { xSorted = []
        , ySorted = []
        }


{-| Return the minimal bounding box such that it contains all the elements in an index
-}
maxBoundingBox : SpatialIndex -> BoundingBox2d
maxBoundingBox index =
    Debug.crash "TBD"


{-| Construct an index from a list of `BoxBoundedValue a`
-}
fromElements : List (BoxBoundedValue a) -> SpatialIndex a
fromElements elements =
    SpatialIndex
        { xSorted = List.sortBy (bounds >> minX) (elements)
        , ySorted = List.sortBy (bounds >> minY) (elements)
        }


{-| Insert a new `BoxBoundedValue` into an index
-}
insert : BoxBoundedValue a -> SpatialIndex a -> SpatialIndex a
insert value index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            SpatialIndex
                { xSorted = List.sortBy (bounds >> minX) (value :: xSorted)
                , ySorted = List.sortBy (bounds >> minY) (value :: ySorted)
                }


{-| Extract all values (without bounding boxes) from all elements in the index.
There are no guarantees on the order of elements
-}
values : SpatialIndex a -> List a
values index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            List.map value xSorted


{-| Return all bounding boxes from all elements in an index.
There are no guarantees on the order of bounding boxes
-}
boundingBoxes : SpatialIndex a -> List BoundingBox2d
boundingBoxes (SpatialIndex { xSorted, ySorted }) =
    List.map bounds xSorted


{-| Return all the elements (`BoxBoundedValue a`) from an index, in no particular order
-}
elements : SpatialIndex a -> List (BoxBoundedValue a)
elements (SpatialIndex { xSorted }) =
    xSorted


{-| Return a new spatial index such that all elements in the new index intersect or contained with the argument `boundingBox`.
-}
span : BoundingBox2d -> SpatialIndex a -> SpatialIndex a
span boundingBox (SpatialIndex { xSorted, ySorted }) =
    let
        { minX, maxX, minY, maxY } =
            extrema boundingBox

        xs =
            xSorted |> List.filter (bounds >> (intersects boundingBox))
    in
        SpatialIndex
            { xSorted = xs
            , ySorted = xs |> List.sortBy (\b -> b |> bounds |> BoundingBox.minY)
            }


{-| Return a new spatial index such that all elements in the new index are contained in the supplied `boundingBox`
-}
containedIn : BoundingBox2d -> SpatialIndex a -> SpatialIndex a
containedIn boundingBox index =
    Debug.crash "TBD"


{-| Return a tuple of two indices which represent the partition of the input index around the supplied line.
The line is represented by a point and the direction of it's normal.
The first result index contains all elements which intersect the line, or contained in the direction of it's normal.
The second result index contains all other elements
-}
partitionByLine : Point2d -> Direction2d -> SpatialIndex a -> ( SpatialIndex a, SpatialIndex a )
partitionByLine point normal index =
    Debug.crash "TBD"


{-| Return a tuple of two indices which represent a partition around a bounding box.
The first index in the result contains all elements of the input index which are contained or intersect the `bounds`.
The second index in the result contains the rest of the elements.
-}
partitionByBounds : BoundingBox2d -> SpatialIndex a -> ( SpatialIndex a, SpatialIndex a )
partitionByBounds bounds index =
    Debug.crash "TBD"


{-| Merge all elements from two indices into a new index. Does not preserve ordering.
-}
merge : SpatialIndex a -> SpatialIndex a -> SpatialIndex a
merge (SpatialIndex xs1) (SpatialIndex xs2) =
    fromElements (.xSorted xs1 |> List.append xs2.xSorted)


{-| Return a list of 2-tuples, each tuple are two `BoxBoundedValue` elements which intersect each other in the index.
Returns all intersecting elements, without duplications and without any guarantee on ordering
-}
collisions : SpatialIndex a -> List ( BoxBoundedValue a, BoxBoundedValue a )
collisions index =
    Debug.crash "TBD"


{-| Apply function `f` on all elements of an index, returning a new index which will be reordered.
-}
map : (BoxBoundedValue a -> BoxBoundedValue b) -> SpatialIndex a -> SpatialIndex b
map f index =
    Debug.crash "TBD"


{-| Apply function `f` on all values of an index, returning a new index with the same structure as the original one
(no change in bounding boxes).
-}
mapValues : (a -> b) -> SpatialIndex a -> SpatialIndex b
mapValues f index =
    Debug.crash "TBD"


{-| Given a `point` return an index with the nearest `k` elements in the input index, or less, if there are less than
`k` elements in the index.
-}
nearest : Int -> Point2d -> SpatialIndex a -> SpatialIndex a
nearest k point index =
    Debug.crash "TBD"


{-| Return a new index containing all elements in the input index not contained or intersecting the bounding box.
-}
remove : BoundingBox2d -> SpatialIndex a -> SpatialIndex a
remove bounds index =
    Debug.crash "TBD"
