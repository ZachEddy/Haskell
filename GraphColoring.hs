--Zach Eddy − CS 291 Homework 4.

data Color = Red | Green | Blue | Yellow | Purple | Brown
            deriving (Eq, Enum, Show, Bounded)
data Binding a = CB a Color
            deriving (Eq, Show)
data Edge a = E a a 
            deriving (Show, Eq)
type Graph a = [Edge a]

getBindingName (CB a _) = a --helper that gets a color binding's name.
getBindingColor (CB _ a) = a --helper that gets a color binding's color.

-- finds whether or not something is stored inside a color binding.
colorOf bindingName [] = Nothing
colorOf bindingName (x:xs) = if (getBindingName (x) == bindingName) then (Just (getBindingColor (x))) else (colorOf bindingName xs)

-- some helpers to get the nodes assosiated with an edge. It's used later to find unique nodes.
getFirst (E a _) = a 
getSecond (E _ b) = b

-- a recursive funciton that determines whether or not two adjacent nodes share the same color.
nodesClash [] edge = False
nodesClash [x] edge = False
nodesClash bindings edge
    |colorOf (getFirst edge) bindings == Nothing = False
    |colorOf (getSecond edge) bindings == Nothing = False
    |otherwise = (binding1 == binding2)
    where
        (Just binding1) = (colorOf (getFirst edge) bindings)
        (Just binding2) = (colorOf (getSecond edge) bindings)

-- pulls the nodes out of an edge and puts them in a list.
getNodes (E a b) = [a,b]

-- finds a list of unique nodes in a graph.
uniqueNodes graph = 
    let
        nodeList = foldl(\acc x -> acc ++ getNodes x) [] graph
    in 
        foldl(\acc x -> if elem x acc then acc else (acc ++ [x])) [] nodeList

-- determines whether or not two adjacent nodes in the entire graph share a color. In other words, if two nodes of the same color share an edge, this returns false. 
noClashes bindings graph = length (filter (True&&) (map(nodesClash bindings) graph)) == 0

-- recursively assigns colors to each node (vertex) in the graph
assignColors [x] graph = [CB x (minBound::Color)]
assignColors (x:xs) graph = let
        assignedColors = (assignColors xs graph)
        totalColors = [(minBound::Color)..(maxBound::Color)]
        possibleColors = filter(\color -> (noClashes((CB x color):assignedColors) graph)) totalColors
        in (CB x (head possibleColors)) : assignedColors

-- basically the main function that ties everything together
colorGraph graph = assignColors (uniqueNodes graph) graph

-- large list of states for test purposes
us = [E "wa" "or", E "or" "ca", E "wa" "id", E "or" "id", E "ca" "nv", E "id" "nv", E "or" "nv", E "id" "mt", E "id" "wy", E "id" "ut", E "nv" "ut", E "nv" "az", E "ca" "az", E "ut" "az", E "mt" "wy", E "wy" "ut", E "wy" "co", E "co" "ut",E "co" "nm", E "nm" "az", E "mt" "nd", E "mt" "sd", E "nd" "mn", E "nd" "sd", E"sd" "wy", E "sd" "ne", E "ne" "co", E "sd" "mn", E "sd" "ia", E "ne" "ia", E "ne" "ks", E "ne" "mo", E "nm" "tx", E "nm" "ok", E "tx" "ok", E "co" "ok", E "co" "ks", E "ks" "ok", E "ks" "mo", E "ok" "ar", E "ar" "la", E "ar" "tx", E "mo" "ar", E "ia" "mo", E "mn" "ia"]
Status API Training Shop Blog About Pricing
