module Spatial2DIndexing exposing (
    SpatialIndex, BoundingBox, box, getMinX, getMaxX, getMinY, getMaxY, equal, compareMinX, compareMaxX, compareMinY, compareMaxY, compareMinXY,
    BoxBoundedValue, getBoundingBox, getValue, boxValue,
    empty, insert, values, boundingBoxes, span
    )


import Tuple exposing (first, second)
import List.Extra exposing (dropWhile, takeWhile)


type SpatialIndex a
    = SpatialIndex
        { xSorted: List (BoxBoundedValue a)
        , ySorted: List (BoxBoundedValue a)
        }


type BoundingBox
    = BoundingBox
        { minX: Float
        , maxX: Float
        , minY: Float
        , maxY: Float
        }


box : Float -> Float -> Float -> Float -> BoundingBox
box minX maxX minY maxY =
    BoundingBox { minX= min minX maxX
        , maxX= max minX maxX
        , minY = min minY maxY
        , maxY = max minY maxY
        }


getXInterval : BoundingBox -> (Float, Float)
getXInterval box =
    case box of
        BoundingBox {minX, maxX} -> (minX, maxX)


getYInterval : BoundingBox -> (Float, Float)
getYInterval box =
    case box of
        BoundingBox {minY, maxY} -> (minY, maxY)


getMinX : BoundingBox -> Float
getMinX box =
    box |> getXInterval >> first


getMaxX : BoundingBox -> Float
getMaxX box =
    box |> getXInterval >> second


getMinY : BoundingBox -> Float
getMinY box =
    box |> getYInterval >> first


getMaxY : BoundingBox -> Float
getMaxY box =
    box |> getYInterval >> second


getCorners : BoundingBox -> (Float, Float, Float, Float)
getCorners box =
    (getMinX box, getMaxX box, getMinY box, getMaxY box)


equal : BoundingBox -> BoundingBox -> Bool
equal a b =
    case a of
        BoundingBox unboxedA ->
            case b of
                BoundingBox unboxedB ->
                    unboxedA.minX == unboxedB.minX
                    && unboxedA.maxX == unboxedB.maxX
                    && unboxedA.minY == unboxedB.minY
                    && unboxedA.maxY == unboxedB.maxY


compareMinX : BoundingBox -> BoundingBox -> Order
compareMinX a b =
    compare (getMinX a) (getMinX b)


compareMaxX : BoundingBox -> BoundingBox -> Order
compareMaxX a b =
    compare (getMaxX a) (getMaxX b)


compareMinY : BoundingBox -> BoundingBox -> Order
compareMinY a b =
    compare (getMinY a) (getMinY b)


compareMaxY : BoundingBox -> BoundingBox -> Order
compareMaxY a b =
    compare (getMaxY a) (getMaxY b)


compareMinXY : BoundingBox -> BoundingBox -> Order
compareMinXY a b =
    case compareMinX a b of
        EQ ->
            compareMinY a b
        LT -> LT
        GT -> GT


intersectX : BoundingBox -> BoundingBox -> Bool
intersectX a b =
    let
        aMin = getMinX a
        bMin = getMinX b
        aMax = getMaxX a
        bMax = getMaxX b
    in
        aMax >= bMin && aMin <= bMax


intersectY : BoundingBox -> BoundingBox -> Bool
intersectY a b =
    let
        aMin = getMinY a
        bMin = getMinY b
        aMax = getMaxY a
        bMax = getMaxY b
    in
        aMax >= bMin && aMin <= bMax


intersect : BoundingBox -> BoundingBox -> Bool
intersect a b =
    intersectX a b && intersectY a b


type BoxBoundedValue a
    = BoxBoundedValue a BoundingBox


boxValue : BoundingBox -> a -> BoxBoundedValue a
boxValue box val =
    BoxBoundedValue val box


getBoundingBox : BoxBoundedValue a -> BoundingBox
getBoundingBox boxed =
    case boxed of
        BoxBoundedValue val box -> box


getValue : BoxBoundedValue a -> a
getValue boxed =
    case boxed of
        BoxBoundedValue val box -> val


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
                { xSorted = List.sortBy (getBoundingBox >> getMinX) (value :: xSorted)
                , ySorted = List.sortBy (getBoundingBox >> getMinY) (value :: ySorted)
                }


values : SpatialIndex a -> List a
values index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            List.map getValue xSorted


boundingBoxes : SpatialIndex a -> List BoundingBox
boundingBoxes index =
    case index of
        SpatialIndex { xSorted, ySorted } ->
            List.map getBoundingBox xSorted


span : BoundingBox -> SpatialIndex a -> SpatialIndex a
span bounds index =
    case index of
        SpatialIndex {xSorted, ySorted} ->
            let
                (minX, maxX, minY, maxY) = getCorners bounds
                xs = xSorted |> List.filter (getBoundingBox >> (intersect bounds))
            in
                SpatialIndex
                    { xSorted = xs
                    , ySorted = xs |> List.sortBy (getBoundingBox >> getMinY)
                    }

