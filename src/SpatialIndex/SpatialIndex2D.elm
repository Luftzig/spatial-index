module SpatialIndex.SpatialIndex2D
    exposing
        ( SpatialIndex
        , BoxBoundedValue
        , getBoundingBox
        , getValue
        , boxValue
        , empty
        , insert
        , values
        , boundingBoxes
        , span
        )

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


boxValue : BoundingBox2d -> a -> BoxBoundedValue a
boxValue box val =
    ( box, val )


getBoundingBox : BoxBoundedValue a -> BoundingBox2d
getBoundingBox ( box, value ) =
    box


getValue : BoxBoundedValue a -> a
getValue ( box, value ) =
    value


empty : SpatialIndex a
empty =
    SpatialIndex
        { xSorted = []
        , ySorted = []
        }


insert : BoxBoundedValue a -> SpatialIndex a -> SpatialIndex a
insert value index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            SpatialIndex
                { xSorted = List.sortBy (getBoundingBox >> minX) (value :: xSorted)
                , ySorted = List.sortBy (getBoundingBox >> minY) (value :: ySorted)
                }


values : SpatialIndex a -> List a
values index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            List.map getValue xSorted


boundingBoxes : SpatialIndex a -> List BoundingBox2d
boundingBoxes (SpatialIndex { xSorted, ySorted }) =
    List.map getBoundingBox xSorted


span : BoundingBox2d -> SpatialIndex a -> SpatialIndex a
span bounds (SpatialIndex { xSorted, ySorted }) =
    let
        { minX, maxX, minY, maxY } =
            extrema bounds

        xs =
            xSorted |> List.filter (getBoundingBox >> (intersects bounds))
    in
        SpatialIndex
            { xSorted = xs
            , ySorted = xs |> List.sortBy (\b -> b |> getBoundingBox |> BoundingBox.minY)
            }
