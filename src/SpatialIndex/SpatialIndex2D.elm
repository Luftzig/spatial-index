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
        , bounds
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


type SpatialIndex a
    = SpatialIndex
        { xSorted : List (BoxBoundedValue a)
        , ySorted : List (BoxBoundedValue a)
        }


type alias BoxBoundedValue a =
    ( BoundingBox2d, a )


element : BoundingBox2d -> a -> BoxBoundedValue a
element box val =
    ( box, val )


bounds : BoxBoundedValue a -> BoundingBox2d
bounds ( box, value ) =
    box


value : BoxBoundedValue a -> a
value ( box, value ) =
    value


empty : SpatialIndex a
empty =
    SpatialIndex
        { xSorted = []
        , ySorted = []
        }


maxBoundingBox : SpatialIndex -> BoundingBox2d
maxBoundingBox index =
    Debug.crash "TBD"


fromElements : List (BoxBoundedValue a) -> SpatialIndex a
fromElements elements =
    SpatialIndex
        { xSorted = List.sortBy (bounds >> minX) (elements)
        , ySorted = List.sortBy (bounds >> minY) (elements)
        }


insert : BoxBoundedValue a -> SpatialIndex a -> SpatialIndex a
insert value index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            SpatialIndex
                { xSorted = List.sortBy (bounds >> minX) (value :: xSorted)
                , ySorted = List.sortBy (bounds >> minY) (value :: ySorted)
                }


values : SpatialIndex a -> List a
values index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            List.map value xSorted


bounds : SpatialIndex a -> List BoundingBox2d
bounds (SpatialIndex { xSorted, ySorted }) =
    List.map bounds xSorted


elements : SpatialIndex a -> List (BoxBoundedValue a)
elements (SpatialIndex { xSorted }) =
    xSorted



-- TODO: Missing: boundingBox to return the bounding box of the whole span


span : BoundingBox2d -> SpatialIndex a -> SpatialIndex a
span bounds (SpatialIndex { xSorted, ySorted }) =
    let
        { minX, maxX, minY, maxY } =
            extrema bounds

        xs =
            xSorted |> List.filter (bounds >> (intersects bounds))
    in
        SpatialIndex
            { xSorted = xs
            , ySorted = xs |> List.sortBy (\b -> b |> bounds |> BoundingBox.minY)
            }


containedIn : BoundingBox2d -> SpatialIndex a -> SpatialIndex a
containedIn range index =
    Debug.crash "TBD"


partitionByLine : Point2d -> Direction2d -> SpatialIndex a -> ( SpatialIndex a, SpatialIndex a )
partitionByLine point normal index =
    Debug.crash "TBD"


partitionByBounds : BoundingBox2d -> SpatialIndex a -> SpatialIndex a
partitionByBounds bounds index =
    Debug.crash "TBD"


merge : SpatialIndex a -> SpatialIndex a -> SpatialIndex a
merge (SpatialIndex xs1) (SpatialIndex xs2) =
    fromElements (.xSorted xs1 |> List.append xs2.xSorted)


collisions : SpatialIndex a -> List ( BoxBoundedValue a, BoxBoundedValue a )
collisions index =
    Debug.crash "TBD"


map : (BoxBoundedValue a -> BoxBoundedValue b) -> SpatialIndex a -> SpatialIndex b
map f index =
    Debug.crash "TBD"


mapValues : (a -> b) -> SpatialIndex a -> SpatialIndex b
mapValues f index =
    Debug.crash "TBD"


nearest : Int -> Point2d -> SpatialIndex a -> SpatialIndex a
nearest k point index =
    Debug.crash "TBD"


remove : BoundingBox2d -> SpatialIndex a -> SpatialIndex a
remove bounds index =
    Debug.crash "TBD"
