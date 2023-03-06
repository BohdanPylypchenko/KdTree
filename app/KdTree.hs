module KdTree where

import Data.List

-- Point type
data Point = EmptyPoint
           | Point [Double]

-- KdTree type
data KdTree = EmptyKdTree
            | KdTree Int Point KdTree KdTree

-- Point dimension getter
getPointDim :: Point -> Int
getPointDim EmptyPoint     = 0
getPointDim (Point coords) = length coords

-- Point coordinate getter
getCoord :: Point -> Int -> Double
getCoord EmptyPoint _  = 0
getCoord (Point coords) k
    | k >= 0 && k < length coords = coords !! k
    | otherwise = error "Index out of range"

-- Tree dimension getter
getKdTreeDim :: KdTree -> Int
getKdTreeDim EmptyKdTree        = 0
getKdTreeDim (KdTree dim _ _ _) = dim

-- tree point getter
getPointAtRoot :: KdTree -> Point
getPointAtRoot EmptyKdTree          = EmptyPoint
getPointAtRoot (KdTree _ point _ _) = point

-- left getter
getLeft :: KdTree -> KdTree
getLeft EmptyKdTree         = EmptyKdTree
getLeft (KdTree _ _ left _) = left

-- right getter
getRight :: KdTree -> KdTree
getRight EmptyKdTree          = EmptyKdTree
getRight (KdTree _ _ _ right) = right

-- Overwriting Show for Point type
instance Show Point where
    show EmptyPoint     = "Empty point"
    show (Point coords) = "Point<"             ++
                          show (length coords) ++
                          "> "                 ++
                          show coords

-- Overwriting Eq for Point type
instance Eq Point where
    EmptyPoint    == EmptyPoint    = True
    EmptyPoint    == Point coords  = False
    Point coords  == EmptyPoint    = False
    Point coords1 == Point coords2 = coords1 == coords2

-- Overwriting Show for KdTree type
instance Show KdTree where
    show (KdTree dim point left right) = kdTreeToString (KdTree dim point left right) "" where
       kdTreeToString EmptyKdTree tabs                   = tabs ++ "Empty KdTree<" ++ show dim ++ ">"
       kdTreeToString (KdTree dim point left right) tabs =
          let nextTabs = tabs ++ "    "
          in
              tabs ++ show point ++
              "\n" ++ tabs ++ "left:\n" ++ kdTreeToString left nextTabs ++
              "\n" ++ tabs ++ "right:\n" ++ kdTreeToString right nextTabs

-- Adds given point to KdTree point
addPointToKdTree :: Point -> KdTree -> KdTree
addPointToKdTree point tree = add point tree 0 where
    add point EmptyKdTree _ = KdTree (getPointDim point) point EmptyKdTree EmptyKdTree
    add point tree k
        | getCoord point k <= getCoord (getPointAtRoot tree) k =
            KdTree (getKdTreeDim tree) (getPointAtRoot tree) (add point (getLeft tree) (kNext k (getKdTreeDim tree))) (getRight tree)
        | getCoord point k > getCoord (getPointAtRoot tree) k  =
            KdTree (getKdTreeDim tree) (getPointAtRoot tree) (getLeft tree) (add point (getRight tree) (kNext k (getKdTreeDim tree)))
        where
            kNext cdr dim = mod (cdr + 1) dim

-- Checks if point is in the tree
isPointInKdTree :: Point -> KdTree -> Bool
isPointInKdTree point tree = check point tree 0 where
    check point EmptyKdTree _ = False
    check point tree k
        | point == getPointAtRoot tree                         = True
        | getCoord point k <= getCoord (getPointAtRoot tree) k = check point (getLeft tree) (kNext k (getKdTreeDim tree))
        | getCoord point k > getCoord (getPointAtRoot tree) k  = check point (getRight tree) (kNext k (getKdTreeDim tree))
        where
            kNext cdr dim = mod (cdr + 1) dim

-- Returns list of tree points
listFromKdTree :: KdTree -> [Point]
listFromKdTree EmptyKdTree = []
listFromKdTree tree = [getPointAtRoot tree]         ++
                      listFromKdTree (getLeft tree) ++
                      listFromKdTree (getRight tree)

-- Creates KdTree with given list of points
kdTreeFromList :: [Point] -> KdTree
kdTreeFromList [] = EmptyKdTree
kdTreeFromList points = treeFromList points EmptyKdTree
    where
        treeFromList points tree
            | null points = tree
            | otherwise   = treeFromList (tail points) (addPointToKdTree (head points) tree)

-- adds (if not present), or removes (if present ) point from tree
addOrRemovePointFromKdTree :: Point -> KdTree -> KdTree
addOrRemovePointFromKdTree point tree
    | isPointInKdTree point tree = kdTreeFromList (delete point (listFromKdTree tree))
    | otherwise = addPointToKdTree point tree
