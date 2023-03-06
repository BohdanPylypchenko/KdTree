module Main where

import KdTree

main :: IO ()
main = do
    let tree = addPointToKdTree (Point [8, 1])
             $ addPointToKdTree (Point [4, 7])
             $ addPointToKdTree (Point [2, 3])
             $ addPointToKdTree (Point [9, 6])
             $ addPointToKdTree (Point [5, 4])
             $ addPointToKdTree (Point [7, 2])
             $ EmptyKdTree

    putStrLn "\nOriginal tree:\n"
    print tree

    putStrLn "\ntree after remove [7, 2]\n"
    let tree1 = addOrRemovePointFromKdTree (Point [7, 2]) tree
    print tree1

    putStrLn "\nTry to add EmptyPoint:\n"
    print (addOrRemovePointFromKdTree EmptyPoint tree1)

    putStrLn "\ntree after remove [4, 7]\n"
    let tree2 = addOrRemovePointFromKdTree (Point [4, 7]) tree1
    print tree2

    putStrLn "\ntree after add [7, 2] [4, 7]\n"
    print (addOrRemovePointFromKdTree (Point [7, 2])
         $ addOrRemovePointFromKdTree (Point [4, 7])
         $ tree2)
