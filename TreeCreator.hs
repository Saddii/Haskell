--Asci signs ╔,╚
--Example of use :
-- -createMAXTree 5
-- -createDeepTree 5
--Node(Node (Leaf 2) 4 (Node (Node (Leaf 14) 13 (Leaf 15)) 16 (Node (Leaf 100) (-6) (Leaf 10)))) 10 (Node (Leaf 10) 20 (Node (Leaf 5) 0 (Node (Leaf (-5)) 6 (Node (Node (Node (Empty) 100 (Empty)) 3 (Empty)) 0 (Node (Leaf 11) (-5) (Leaf 10))) )))

data BTree a = Empty | Leaf a | Node (BTree a) a (BTree a)
instance (Show a) => Show (BTree a) where
  show x = showWithDepth x 0 "root" "root" []
showWithDepth :: (Show a) => BTree a -> Int -> [Char] -> [Char] -> [Int] -> String

--Empty
showWithDepth Empty depth l_r rodzic list = if l_r /= rodzic then
    (if l_r =="r" then
       space  else (if l_r == "l" then
           space  else " ")) ++ "   "
    else (if l_r =="r" then
       (kreski (init(list)) (depth-2) 0) else (if l_r == "l" then
           (kreski (init(list)) (depth-3) 0) else " ")) ++ "   "
   where
       space = (kreski (list) (depth-3) 0)
--Leaf
showWithDepth (Leaf e) depth l_r rodzic list =if l_r /= rodzic then
     (if l_r =="r" then
        space ++ " /" else (if l_r == "l" then
            space ++ " \\" else " ")) ++ show e
     else (if l_r =="r" then
        (kreski (init(list)) (depth-2) 0) ++ " /" else (if l_r == "l" then
            (kreski (init(list)) (depth-3) 0) ++ "  \\" else " ")) ++ show e 
    where
        space = (kreski (list) (depth-3) 0)
        
--Root
showWithDepth (Node ls e rs) depth "root" _ list = showWithDepth rs (depth + 2) "r" "r" [1] ++ "\n"   ++ "(" ++ show e ++ ")" ++ "\n"  ++ showWithDepth ls (depth+2) "l" "l" [1]
--Node
showWithDepth (Node ls e rs) depth l_r rodzic list =
    if rodzic == l_r then 
        if l_r == "r" then
            showWithDepth rs (depth + 2) "r" "r" list1  ++ "\n" ++ space1 ++ "╔" ++ node ++ "\n"  ++ showWithDepth ls (depth+2) "l" "r" list1 
        else 
        showWithDepth rs (depth + 2) "r" "l" list1  ++ "\n" ++ space1 ++ "╚" ++ node ++ "\n"  ++ showWithDepth ls (depth+2) "l" "l" list1
    else
        if l_r == "r" then
            showWithDepth rs (depth + 2) "r" "r" list2 ++ "\n" ++ space2 ++ "╔" ++ node ++ "\n"  ++ showWithDepth ls (depth+2) "l" "r" list2
        else 
        showWithDepth rs (depth + 2) "r" "l" list2  ++ "\n" ++ space2 ++ "╚" ++ node ++ "\n"  ++ showWithDepth ls (depth+2) "l" "l" list2 
    where
        space1=(kreski (init(list)) (depth-1) 0)
        list1 = (init(list)++[depth-1])
        space2 = (kreski (list) (depth-1) 0)
        list2 = (list++[depth-1])
        node = "(" ++ show e ++")"

kreski [] d n = replicate (d-n) ' '
kreski (x:xs) d n  = if x == n then "|" ++ (kreski xs d (n+1)) else
    " " ++ (kreski (x:xs) d (n+1))



--Generators------------------------------------------------------------------------------------
--example: 
-- -createMAXTree 5
-- -createDeepTree 5
createMAXTree :: Int -> BTree Int
createMAXTree 0 = Leaf 1
createMAXTree depth = Node (createMAXTree (depth - 1)) depth (createMAXTree (depth - 1))

createDeepTree n = createDeepTreeHelp n "root"
createDeepTreeHelp :: Int -> [Char] -> BTree Int
createDeepTreeHelp 0 _ = Leaf 1
createDeepTreeHelp depth "root" = Node (createDeepTreeHelp (depth - 1) "a") depth (createDeepTreeHelp (depth - 1) "b")
createDeepTreeHelp depth x
    | depth `mod` 2 == 0 = Node (createDeepTreeHelp (depth - 1) x) depth (Leaf 0)
    | otherwise          = Node (Leaf 0) depth (createDeepTreeHelp (depth - 1) x)

