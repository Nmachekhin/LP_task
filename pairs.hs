module Pairs where
import Data.List (delete, nub)

data State = State { leftH :: [Int], leftW :: [Int], rightH :: [Int], rightW :: [Int], boat :: Bool }
    deriving (Eq, Show)

initialState :: State
initialState = State [1,2,3] [1,2,3] [] [] True

goalState :: State -> Bool
goalState (State [] [] rh rw False) = length rh==3 && length rw==3
goalState _ = False

safeBank :: [Int] -> [Int] -> Bool
safeBank h w = null w || null h || all (`elem` h) w

safeState :: State -> Bool
safeState (State lh lw rh rw b)  = safeBank rh rw && safeBank lh lw


move :: State -> [State]
move (State lh lw rh rw True) = do
    passengers <- selectPassengers lh lw
    let (lh', lw') = removePeople passengers lh lw
        (rh', rw') = addPeople passengers rh rw
    let newState = State lh' lw' rh' rw' False
    if safeState newState then [newState] else []
move (State lh lw rh rw False) = do
    passengers <- selectPassengers rh rw
    let (rh', rw') = removePeople passengers rh rw
        (lh', lw') = addPeople passengers lh lw
    let newState = State lh' lw' rh' rw' True
    if safeState newState then [newState] else []

selectPassengers :: [Int] -> [Int] -> [[(Char, Int)]]
selectPassengers h w = 
    [[('H', p)] | p <- h] ++
    [[('W', p)] | p <- w] ++
    [[('H', p1), ('H', p2)] | p1 <- h, p2 <- h, p1 < p2] ++
    [[('W', p1), ('W', p2)] | p1 <- w, p2 <- w, p1 < p2] ++
    [[('H', p), ('W', p)] | p <- h, p `elem` w]

removePeople :: [(Char, Int)] -> [Int] -> [Int] -> ([Int], [Int])
removePeople passengers h w = 
    (foldl (flip delete) h [p | ('H', p) <- passengers],
     foldl (flip delete) w [p | ('W', p) <- passengers])

addPeople :: [(Char, Int)] -> [Int] -> [Int] -> ([Int], [Int])
addPeople passengers h w = 
    (foldl (\acc p -> p:acc) h [p | ('H', p) <- passengers],
     foldl (\acc p -> p:acc) w [p | ('W', p) <- passengers])

bfs :: [[State]] -> [State] -> Maybe [State]
bfs [] _ = Nothing
bfs (path@(s:_):paths) visited
    | goalState s = Just (reverse path)
    | otherwise   = bfs (paths ++ newPaths) (s:visited)
  where
    newStates = filter (`notElem` visited) (move s)
    newPaths = [ns : path | ns <- newStates]

solve :: Maybe [State]
solve = bfs [[initialState]] []

