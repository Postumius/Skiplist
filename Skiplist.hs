{- cabal:
ghc-options: -Wunused-imports
build-depends: base, random, mtl, transformers
-}

import Data.List
import Data.Function
import System.Random
import Control.Monad.State

infixr 8 .-
(.-) f g = \x -> g (f x)

data (Ord a) => Between a = Min | Val a | Max
  deriving (Eq)

instance (Ord a) => Ord (Between a) where
  (<=) Min _ = True
  (<=) _ Min = False
  (<=) _ Max = True
  (<=) Max _ = False
  (<=) (Val x) (Val y) = x <= y

instance (Ord a, Show a) => Show (Between a) where
  show Min = "|"
  show (Val a) = show a
  show Max = "Max"

data (Ord a) => Tree a = Fork (Tree a) [(a, Tree a)] | Leaf [a]

intercalateS sep = intersperse sep .- foldr (.) id

instance (Ord a, Show a) => Show (Tree a) where
  show tree = 
    let lines = tree & toLines & snd & map ((" "++) .)
    in ("\n"++) . intercalateS ("\n"++) lines $ "\n"

listLevel (Leaf vs) = vs
  & map Just
  & (`zip` (repeat Nothing))
  & ((Nothing, Nothing) :)
listLevel (Fork min branches) = branches
  & map (\(val, tree) -> (Just val, Just tree))
  & ((Nothing, Just min) :)

toLines :: (Ord a, Show a) => Tree a -> (Int, [ShowS])
toLines (Leaf vs) =
  let line = vs 
        & map show 
        & intercalate "_"
        & ("|_" ++)
        & (++ " ")
  in (length line, [(line++)])
toLines fork@(Fork _ branches) = fork
  & listLevel
  & zip (reverse [0..length branches])
  & map ( \(i, (mVal, Just tree)) ->
    let (levelWidth, lowerLevels) = toLines tree
        valStr = case mVal of
          Nothing -> "|"
          Just val -> show val
        valStrWidth = length valStr
        thisLevel = 
          (valStr++) . 
          ( replicate (levelWidth-valStrWidth)
                      (if i == 0 then ' ' else '_') 
            ++ )
    in (levelWidth, thisLevel:lowerLevels) )
  & foldr (\(width, levels) (accWidth, accLevels) -> 
      ( width + accWidth
      , zip levels accLevels & map (uncurry (.)) ))
     (0, repeat id)

type MBranch a = (Maybe a, Maybe (Tree a))

data Trail a = 
  BackUp (Maybe a) [MBranch a] | 
  BackLeft (MBranch a)
  deriving (Show)

type JacobLadder a = ([MBranch a], [Trail a])

climbInto tree = (listLevel tree, [])

climbDown ((mVal, Just tree):bs, trail) = (listLevel tree, BackUp mVal bs : trail)

climbRight (b:bs, trail) = (bs, BackLeft b: trail)

treeLevel ((Nothing, Nothing) : bs) = bs 
  & map (\(Just val, Nothing) -> val) 
  & Leaf
treeLevel ((Nothing, Just minTree) : bs) = bs
  & map (\(Just val, Just tree) -> (val, tree))
  & Fork minTree

goBackOne (level, BackUp mVal bs : trail) = ((mVal,treeLevel level & Just) : bs, trail) 
goBackOne (bs, BackLeft b : trail) = (b:bs, trail)

goBackUp (level, []) = goBackOne (level, [BackUp Nothing []])
goBackUp ladder@(_, BackUp _ _ : _) = goBackOne ladder
goBackUp ladder = goBackOne ladder & goBackUp

climbOutOf (level, []) = treeLevel level
climbOutOf ladder = climbOutOf $ goBackOne ladder

lookFor fruit ladder@((mVal,mTree):bs, _)
  | Just fruit == mVal = ladder
  | null bs || Just fruit < fst (head bs) = case mTree of
    Nothing -> ladder
    Just _ -> ladder & climbDown & lookFor fruit
  | otherwise = ladder & climbRight & lookFor fruit 

coinFlip :: (RandomGen g) => State g Bool
coinFlip = state random

coinPromote :: (Ord a, RandomGen g) => JacobLadder a -> State g (JacobLadder a)
coinPromote ladder@((mVal,mTree):bs, trail) = do
  coin <- coinFlip
  if coin
  then let (a:as, trail') = ([], trail) & goBackUp
           subTree = (Nothing, mTree):bs & treeLevel
       in (a:(mVal, Just subTree):as, trail')
          & climbRight
          & coinPromote
  else return ladder

insert :: (Ord a, RandomGen g) => a -> Tree a -> State g (Tree a)
insert fruit tree = do
  let ((mVal, mTree) : bs, trail) = climbInto tree & lookFor fruit 
  let mFruit = Just fruit
  tree' <- if mFruit == mVal
    then ((mFruit, mTree) : bs, trail)
         & return
    else ((mVal, Nothing):(mFruit, Nothing):bs, trail) 
         & climbRight
         & coinPromote
  climbOutOf tree' & return 

fromList :: (Ord a, RandomGen g) => [a] -> State g (Tree a)
fromList = foldr (\val acc -> acc >>= Main.insert val) (return $ Leaf [])

testOnRandoms n = do
  g <- newStdGen
  let ls = randomRs (0,99) g & take n :: [Int]
  show ls & putStrLn
  g' <- newStdGen
  let (tree, g'') = runState (fromList ls) g'
  show tree & putStrLn

testOnList ls = do
  g <- newStdGen
  show ls & putStrLn
  let (tree, g') = runState (fromList ls) g
  show tree & putStrLn
