data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Show)
-- treeHeight намира височината на дърво; използвана от упражнения като treeDepth
treeHeight :: (Num b, Ord b) => Tree a -> b
treeHeight Empty = 0
treeHeight (Node _ left right) = 1 + max (treeHeight left) (treeHeight right)

--
isBalanced :: Tree a -> Int -> Bool
isBalanced Empty _ = True
isBalanced tree@(Node v left right) k = isBalanced left k && isBalanced right k && ((treeHeight left) - (treeHeight right) <= k)
