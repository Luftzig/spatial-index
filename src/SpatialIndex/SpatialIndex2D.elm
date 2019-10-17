module SpatialIndex.SpatialIndex2D exposing
    ( BoxBoundedValue
    , SpatialIndex
    , boundingBoxes
    , bounds
    , collisions
    , containedIn
    , element
    , elements
    , empty
    , fromElements
    , insert
    , map
    , mapValues
    , maxBoundingBox
    , merge
    , nearest
    , partitionByBounds
    , partitionByLine
    , remove
    , span
    , value
    , values
    )

import BoundingBox2d as BoundingBox exposing (BoundingBox2d, extrema, intersects, minX, minY)
import Direction2d exposing (Direction2d)
import List.Extra exposing (dropWhile, takeWhile)
import Point2d exposing (Point2d)
import Quantity exposing (Quantity)
import Tuple exposing (first, second)


{-| A Two dimensional spatial index
a - the values stored in the index
quantity - unit used for measure
coordinates - coordinates system used
-}
type SpatialIndex quantity coordinates a
    = SpatialIndex
        { xSorted : List (BoxBoundedValue quantity coordinates a)
        , ySorted : List (BoxBoundedValue quantity coordinates a)
        }


{-| An element of an index, made up from a value `a` and a `BoundingBox2d`
-}
type alias BoxBoundedValue quantity coordinates a =
    ( BoundingBox2d quantity coordinates, a )


{-| Bind a value `a` with a bounding box to create a valid index value.
-}
element : BoundingBox2d quantity coordinates -> a -> BoxBoundedValue quantity coordinates a
element box val =
    ( box, val )


{-| Extract the bounding box out of an index element
-}
bounds : BoxBoundedValue quantity coordinates a -> BoundingBox2d quantity coordinates
bounds ( box, _ ) =
    box


{-| Extract the value out of an index element
-}
value : BoxBoundedValue q c a -> a
value ( _, val ) =
    val


{-| Construct an empty index
-}
empty : SpatialIndex q c a
empty =
    SpatialIndex
        { xSorted = []
        , ySorted = []
        }


{-| Return the minimal bounding box such that it contains all the elements in an index
-}
maxBoundingBox : SpatialIndex q c a -> BoundingBox2d q c
maxBoundingBox index =
    Debug.todo "TBD"


getRawQuantity : Quantity number unit -> number
getRawQuantity (Quantity.Quantity n) =
    n


{-| Construct an index from a list of `BoxBoundedValue a`
-}
fromElements : List (BoxBoundedValue q c a) -> SpatialIndex q c a
fromElements inputs =
    SpatialIndex
        { xSorted = List.sortBy (bounds >> minX >> getRawQuantity) inputs
        , ySorted = List.sortBy (bounds >> minY >> getRawQuantity) inputs
        }


{-| Insert a new `BoxBoundedValue` into an index
-}
insert : BoxBoundedValue q c a -> SpatialIndex q c a -> SpatialIndex q c a
insert val index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            SpatialIndex
                { xSorted = List.sortBy (bounds >> minX >> getRawQuantity) (val :: xSorted)
                , ySorted = List.sortBy (bounds >> minY >> getRawQuantity) (val :: ySorted)
                }


{-| Extract all values (without bounding boxes) from all elements in the index.
There are no guarantees on the order of elements
-}
values : SpatialIndex q c a -> List a
values index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            List.map value xSorted


{-| Return all bounding boxes from all elements in an index.
There are no guarantees on the order of bounding boxes
-}
boundingBoxes : SpatialIndex q c a -> List (BoundingBox2d q c)
boundingBoxes (SpatialIndex { xSorted, ySorted }) =
    List.map bounds xSorted


{-| Return all the elements (`BoxBoundedValue a`) from an index, in no particular order
-}
elements : SpatialIndex q c a -> List (BoxBoundedValue q c a)
elements (SpatialIndex { xSorted }) =
    xSorted


{-| Return a new spatial index such that all elements in the new index intersect or contained with the argument `boundingBox`.
-}
span : BoundingBox2d q c -> SpatialIndex q c a -> SpatialIndex q c a
span boundingBox (SpatialIndex { xSorted, ySorted }) =
    let
        { minX, maxX, minY, maxY } =
            extrema boundingBox

        xs =
            xSorted |> List.filter (bounds >> intersects boundingBox)
    in
    SpatialIndex
        { xSorted = xs
        , ySorted = xs |> List.sortBy (\b -> b |> bounds |> BoundingBox.minY |> getRawQuantity)
        }


{-| Return a new spatial index such that all elements in the new index are contained in the supplied `boundingBox`
-}
containedIn : BoundingBox2d q c -> SpatialIndex q c a -> SpatialIndex q c a
containedIn boundingBox index =
    Debug.todo "TBD"


{-| Return a tuple of two indices which represent the partition of the input index around the supplied line.
The line is represented by a point and the direction of it's normal.
The first result index contains all elements which intersect the line, or contained in the direction of it's normal.
The second result index contains all other elements
-}
partitionByLine : Point2d q c -> Direction2d c -> SpatialIndex q c a -> ( SpatialIndex q c a, SpatialIndex q c a )
partitionByLine point normal index =
    Debug.todo "TBD"


{-| Return a tuple of two indices which represent a partition around a bounding box.
The first index in the result contains all elements of the input index which are contained or intersect the `bounds`.
The second index in the result contains the rest of the elements.
-}
partitionByBounds : BoundingBox2d q c -> SpatialIndex q c a -> ( SpatialIndex q c a, SpatialIndex q c a )
partitionByBounds inBounds index =
    Debug.todo "TBD"


{-| Merge all elements from two indices into a new index. Does not preserve ordering.
-}
merge : SpatialIndex q c a -> SpatialIndex q c a -> SpatialIndex q c a
merge (SpatialIndex xs1) (SpatialIndex xs2) =
    fromElements (.xSorted xs1 |> List.append xs2.xSorted)


{-| Return a list of 2-tuples, each tuple are two `BoxBoundedValue` elements which intersect each other in the index.
Returns all intersecting elements, without duplications and without any guarantee on ordering
-}
collisions : SpatialIndex q c a -> List ( BoxBoundedValue q c a, BoxBoundedValue q c a )
collisions index =
    Debug.todo "TBD"


{-| Apply function `f` on all elements of an index, returning a new index which will be reordered.
-}
map : (BoxBoundedValue q c a -> BoxBoundedValue q c b) -> SpatialIndex q c a -> SpatialIndex q c b
map f index =
    Debug.todo "TBD"


{-| Apply function `f` on all values of an index, returning a new index with the same structure as the original one
(no change in bounding boxes).
-}
mapValues : (a -> b) -> SpatialIndex q c a -> SpatialIndex q c b
mapValues f index =
    Debug.todo "TBD"


{-| Given a `point` return an index with the nearest `k` elements in the input index, or less, if there are less than
`k` elements in the index.
-}
nearest : Int -> Point2d q c -> SpatialIndex q c a -> SpatialIndex q c a
nearest k point index =
    Debug.todo "TBD"


{-| Return a new index containing all elements in the input index not contained or intersecting the bounding box.
-}
remove : BoundingBox2d q c -> SpatialIndex q c a -> SpatialIndex q c a
remove excludeBounds index =
    Debug.todo "TBD"
