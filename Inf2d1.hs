-- Inf2d Assignment 1 2018-2019
-- Matriculation number: s1704037
-- {-# OPTIONS -Wall #-}


module Inf2d1 where

import Data.List (sortBy)
import Debug.Trace
import TTTGame

gridLength_search::Int
gridLength_search = 6
gridWidth_search :: Int
gridWidth_search = 6



{- NOTES:

-- DO NOT CHANGE THE NAMES OR TYPE DEFINITIONS OF THE FUNCTIONS!
You can write new auxillary functions, but don't change the names or type definitions
of the functions which you are asked to implement.

-- Comment your code.

-- You should submit this file, and only this file, when you have finished the assignment.

-- The deadline is the  13th March 2018 at 3pm.

-- See the assignment sheet and document files for more information on the predefined game functions.

-- See the README for description of a user interface to test your code.

-- See www.haskell.org for haskell revision.

-- Useful haskell topics, which you should revise:
-- Recursion
-- The Maybe monad
-- Higher-order functions
-- List processing functions: map, fold, filter, sortBy ...

-- See Russell and Norvig Chapters 3 for search algorithms,
-- and Chapter 5 for game search algorithms.

-}

-- Section 1: Uniform Search

-- 6 x 6 grid search states

-- The Node type defines the position of the robot on the grid.
-- The Branch type synonym defines the branch of search through the grid.
type Node = (Int,Int)
type Branch = [(Int,Int)]

badNodesList::[Node]
-- This is your list of bad nodes. You should experimet with it to make sure your algorithm covers different cases.
badNodesList = [(1, 1), (2, 2), (3, 3), (4, 4), (5, 5), (6, 6)]

-- The maximum depth this search can reach
-- TODO: Fill in the maximum depth and justify your choice
maxDepth::Int
maxDepth= 35 --
-- Why did you choose this number?
-- YOUR ANSWER GOES HERE It's a 6x6 grid, which means there are 36 cells and one of them is the goal cell so I chose the max depth as 35.


-- The next function should return all the possible continuations of input search branch through the grid.
-- Remember that the robot can only move up, down, left and right, and can't move outside the grid.
-- The current location of the robot is the head of the input branch.
-- Your function should return an empty list if the input search branch is empty.
-- This implementation of next function does not backtrace branches.

next::Branch -> [Branch]
next [] = []
next (node:branch) =
  [[(fst node+1, snd node)] ++ node:branch| fst node<6 && not((fst node+1, snd node) `elem` branch) && not((fst node+1, snd node) `elem` badNodesList)]++
  -- Allows the robot to move to the right (with the necessary restrictions: doesn't go off the grid, doesn't go to a node which has already been visited, doesn't go to a 'bad node')
  [[(fst node-1, snd node)] ++ node:branch| fst node>1 && not((fst node-1, snd node) `elem` branch) && not((fst node-1, snd node) `elem` badNodesList)]++
  -- """ Left """
  [[(fst node, snd node+1)] ++ node:branch| snd node<6 && not((fst node, snd node+1) `elem` branch) && not((fst node, snd node+1) `elem` badNodesList)]++
  -- """ Up """
  [[(fst node, snd node-1)] ++ node:branch| snd node>1 && not((fst node, snd node-1) `elem` branch) && not((fst node, snd node-1) `elem` badNodesList)]
  -- """ Down """



-- |The checkArrival function should return true if the current location of the robot is the destination, and false otherwise.
 -- Note that this is the right type declaration for this function. You might have an old version of the Assignment PDF that names this wrongly.
checkArrival::Node -> Node -> Bool
checkArrival destination curNode = if (fst destination == fst curNode && snd destination == snd curNode) then True else False


-- Section 3 Uniformed Search
-- | Breadth-First Search
-- The breadthFirstSearch function should use the next function to expand a node,
-- and the checkArrival function to check whether a node is a destination position.
-- The function should search nodes using a breadth first search order.

breadthFirstSearch::Node->(Branch -> [Branch])->[Branch]->[Node]->Maybe Branch

breadthFirstSearch destination next branches exploredList =
  if length branches == 0 then Nothing -- if EMPTY?(frontier) then return failure
  else if checkArrival destination (head (head branches)) then Just (head branches) -- if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
  else if (head (head branches) `elem` exploredList) then breadthFirstSearch destination next (tail branches) exploredList -- Checks if the node has already been explored

  else breadthFirstSearch destination next newBranches exploredList -- Recursive call on adjusted (FIFO) frontier and exploredList
    where newBranches = (tail branches) ++ (next (head branches)) ; explored = [head(head branches)] ++ exploredList






-- | Depth-First Search
-- The depthFirstSearch function is similiar to the breadthFirstSearch function,
-- except it searches nodes in a depth first search order.
depthFirstSearch::Node->(Branch -> [Branch])->[Branch]-> [Node]-> Maybe Branch

depthFirstSearch destination next branches exploredList =
  if length branches == 0 then Nothing -- if EMPTY?(frontier) then return failure
  else if checkArrival destination (head (head branches)) then Just (head branches) -- if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
  else if (head (head branches) `elem` exploredList) then depthFirstSearch destination next (tail branches) exploredList -- Checks if the node has already been visited

  else depthFirstSearch destination next newBranchlist exploredList -- Recursive call on adjusted (LIFO) frontier and updated exploredList
    where newBranchlist = (next (head branches)) ++ (tail branches) ; explored = [head(head branches)] ++ exploredList

-- | Depth-Limited Search
-- The depthLimitedSearch function is similiar to the depthFirstSearch function,
-- except its search is limited to a pre-determined depth, d, in the search tree.
depthLimitedSearch::Node->(Branch -> [Branch])->[Branch]-> Int-> Maybe Branch
depthLimitedSearch destination next branches d =
  if length branches == 0 then Nothing -- if EMPTY?(frontier) then return failure
  else if checkArrival destination (head (head branches)) then Just (head branches)  -- if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
  else if d == 0 then Nothing -- else if limit = 0 then return cutoff
  else if goal == Nothing then depthLimitedSearch destination next (tail branches) d else goal
    where goal = depthLimitedSearch destination next (next (head branches)) (d - 1) -- recursive function call with the limit decreasing

-- | Iterative-deepening search
-- The iterDeepSearch function should initially search nodes using depth-first to depth d,
-- and should increase the depth by 1 if search is unsuccessful.
-- This process should be continued until a solution is found.
-- Each time a solution is not found the depth should be increased.

--loop :: Int -> Int
--loop n = foldr (\a _ -> a + 1) n [n..maxDepth]

iterDeepSearch:: Node-> (Branch -> [Branch])->Node -> Int-> Maybe Branch
iterDeepSearch destination next initialNode d =
  if goal == Nothing then iterDeepSearch destination next initialNode (d+1)
  else if d >= maxDepth then Nothing else goal
    where goal = depthLimitedSearch destination next [[initialNode]] d
-- | Section 4: Informed search

-- Manhattan distance heuristic
-- This function should return the manhattan distance between the 'position' point and the 'destination'.

manhattan:: Node -> Node -> Int
manhattan position destination = abs(fst destination - fst position) + abs(snd destination - snd position)

-- | Best-First Search
-- The bestFirstSearch function uses the checkArrival function to check whether a node is a destination position,
-- and the heuristic function (of type Node->Int) to determine the order in which nodes are searched.
-- Nodes with a lower heuristic value should be searched before nodes with a higher heuristic value.

bestFirstSearch::Node->(Branch -> [Branch])->(Node->Int)->[Branch]-> [Node]-> Maybe Branch
bestFirstSearch destination next heuristic branches exploredList =
  if length branches == 0 then Nothing -- if succsessors is empty then return failure
  else if checkArrival destination (head (head branches)) then Just (head branches) -- if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
  else if (head (head branches) `elem` exploredList) then bestFirstSearch destination next heuristic (tail successors) exploredList -- checks if the node has already been visited

    else bestFirstSearch destination next heuristic successors exploredList -- recursive call on the succsessors list sorted according to the heuristic function
      where successors =  sortBy (\x y -> compare (heuristic (head x)) (heuristic (head y))) (next (head branches)) ++ (tail branches) ; explored = [head(head branches)] ++ exploredList


-- | A* Search
-- The aStarSearch function is similar to the bestFirstSearch function
-- except it includes the cost of getting to the state when determining the value of the node.

aStarSearch::Node->(Branch -> [Branch])->(Node->Int)->(Branch ->Int)->[Branch]-> [Node]-> Maybe Branch
aStarSearch destination next heuristic cost branches exploredList =
  if length branches == 0 then Nothing -- if succsessors is empty then return failure
  else if checkArrival destination (head (head branches)) then Just (head branches) -- if problem.GOAL-TEST(node.STATE) then return SOLUTION(node)
  else if (head (head branches) `elem` exploredList) then aStarSearch destination next heuristic cost (tail successors) exploredList -- checks if the node has already been visited

  else aStarSearch destination next heuristic cost successors exploredList -- recursive call on the successors list sorted according to the heuristic and the actual cost
    where successors =  sortBy (\x y -> compare ((heuristic (head x)) + (cost (x))) ((heuristic (head y)) + (cost (y)))) (next (head branches)) ++ (tail branches) ; explored = [head(head branches)] ++ exploredList

-- | The cost function calculates the current cost of a trace, where each movement from one state to another has a cost of 1.
cost :: Branch -> Int
cost branch = (length branch) - 1


-- | Section 5: Games
-- See TTTGame.hs for more detail on the functions you will need to implement for both games' minimax and alphabeta searches.



-- | Section 5.1 Tic Tac Toe


-- | The eval function should be used to get the value of a terminal state.
-- A positive value (+1) is good for max player. The human player will be max.
-- A negative value (-1) is good for min player. The computer will be min.
-- A value 0 represents a draw.

eval :: Game -> Int
-- simply checks if player 1 has won, and if so returns 1, else check for player 0 and if so returns -1, else returns 0 as draw
eval game =
  if checkWin game 1 then 1
    else if checkWin game 0 then -1
      else 0


-- | The minimax function should return the minimax value of the state (without alphabeta pruning).
-- The eval function should be used to get the value of a terminal state.

minimax:: Game->Player->Int
minimax game player =
  if (terminal game) then eval game --if node is a terminal node then return the heuristic value of node
    else if maxPlayer player then maximum(map (\x -> minimax x (switch player)) (moves game player)) -- if maximizingPlayer then for each child of node do ...
      else minimum(map (\x -> minimax x (switch player)) (moves game player)) --  if minimizingPlayer then for each child of node do ...




-- | The alphabeta function should return the minimax value using alphabeta pruning.
-- The eval function should be used to get the value of a terminal state.

alphabeta:: Game->Player->Int
alphabeta game player = abrecurse game (-2) 2 player -- calls the abrecurse function with the given alpha and beta values


abrecurse:: Game->Int->Int->Player->Int
abrecurse game a b player =
  if (terminal game) then eval game --if node is a terminal node then return the heuristic value of node
    else if maxPlayer player then pruner (moves game player) a b player --if the player is maximising, calls the pruner function
      else prunerMin (moves game player) a b player --if the player is maximising, calls the prunerMin function

pruner:: [Game]->Int->Int->Player->Int
pruner games a b player = if ((max a (abrecurse (head games) a b (switch player))) >= b)
  then abrecurse (head games) a b (switch player)
    else if (length (tail games) /= 0)
      then (pruner (tail games) (max a (abrecurse (head games) a b (switch player))) b (player))
        else (max a (abrecurse (head games) a b (switch player)))

prunerMin:: [Game]->Int->Int->Player->Int
prunerMin games a b player = if (a >= min b (abrecurse (head games) a b (switch player)))
  then abrecurse (head games) a b (switch player)
    else if (length (tail games) /= 0)
      then (prunerMin (tail games) a (min b (abrecurse (head games) a b (switch player))) (player))
        else min b (abrecurse (head games) a b (switch player))

-- | Section 5.2 Wild Tic Tac Toe





-- | The evalWild function should be used to get the value of a terminal state.
-- It should return 1 if either of the move types is in the correct winning position.
-- A value 0 represents a draw.

evalWild :: Game -> Int
-- simply gives the player who reached(!) the terminal state +1  if either the x's or the o's are in the correct position.
evalWild game =
    if checkWin game 1 then 1
      else if checkWin game 0 then 1
        else 0



-- | The alphabetaWild function should return the minimax value using alphabeta pruning.
-- The evalWild function should be used to get the value of a terminal state. Note that this will now always return 1 for any player who reached the terminal state.
-- You will have to modify this output depending on the player. If a move by the max player sent(!) the game into a terminal state you should give a +1 reward.
-- If the min player sent the game into a terminal state you should give -1 reward.

alphabetaWild:: Game->Player->Int
alphabetaWild game player = undefined



-- | End of official assignment. However, if you want to also implement the minimax function to work for Wild Tic Tac Toe you can have a go at it here. This is NOT graded.


-- | The minimaxWild function should return the minimax value of the state (without alphabeta pruning).
-- The evalWild function should be used to get the value of a terminal state.

minimaxWild:: Game->Player->Int
minimaxWild game player = undefined



  -- | Auxiliary Functions
-- Include any auxiliary functions you need for your algorithms here.
-- For each function, state its purpose and comment adequately.
-- Functions which increase the complexity of the algorithm will not get additional scores
