{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Thief
  ( initGame
  , step
  , turn
  , Game(..)
  , Direction(..)
  , dead, win,gold,golds, enermy,enermy1,enermy2,enermy3,enermy4,barry,score, thief 
  , height, width
  ) where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

-- Types

data Game = Game
  { _thief  :: Thief        -- ^ thief as a sequence of points in N2
  
  , _dir    :: Direction    -- ^ direction
  , _gold   :: Coord        -- ^ location of the gold
  , _enermy   :: Coord        -- ^ location of the enermy
  , _enermy1   :: Coord        -- ^ location of the enermy1
  , _enermy2   :: Coord        -- ^ location of the enermy2
  , _enermy3   :: Coord        -- ^ location of the enermy3
  , _enermy4   :: Coord        -- ^ location of the enermy4
  , _barry  :: Seq Coord        -- ^ location of the barry
  , _golds  :: Seq Coord    -- ^ infinite list of random next gold locations
  , _dead   :: Bool         -- ^ game over flag
  , _win   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool         -- ^ lock to disallow duplicate turns between time steps
  } deriving (Show)

type Coord = V2 Int

type Thief = Seq Coord

data Stream a = a :| Stream a
  deriving (Show)

data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game


height, width :: Int
height = 40
width = 80




step :: Game -> Game
step s = flip execState s . runMaybeT $ do

  MaybeT $ guard . not <$> orM [use paused, use dead, use win]

  MaybeT . fmap Just $ locked .= False

  die <|> kill  <|> kill1 <|> kill2 <|> kill3 <|> kill4 <|> findgold  <|> MaybeT (Just <$> modify move)

die :: MaybeT (State Game) ()
die = do
  MaybeT . fmap guard $ elem <$> (nextHead <$> get) <*> (use barry)
  MaybeT . fmap Just $ dead .= True


kill :: MaybeT (State Game) ()
kill = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use enermy)
  MaybeT . fmap Just $ dead .= True

kill1 :: MaybeT (State Game) ()
kill1 = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use enermy1)
  MaybeT . fmap Just $ dead .= True

kill2 :: MaybeT (State Game) ()
kill2 = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use enermy2)
  MaybeT . fmap Just $ dead .= True

kill3 :: MaybeT (State Game) ()
kill3 = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use enermy3)
  MaybeT . fmap Just $ dead .= True

kill4 :: MaybeT (State Game) ()
kill4 = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use enermy4)
  MaybeT . fmap Just $ dead .= True




findgold :: MaybeT (State Game) ()
findgold = do
  MaybeT . fmap guard $ (==) <$> (nextHead <$> get) <*> (use gold)
  MaybeT . fmap Just $ do
    modifying score (+ 1)
    (>) 10 <$> use score >>= \case
      True -> nextgold
      False -> win .= True




nextgold :: State Game ()
nextgold = do
  fs <- use golds 
  gold .= (fs `S.index` 0)
  golds .= (S.drop 1 fs)
  

move :: Game -> Game
move g@Game { _thief = (s :|> _) } = g & thief .~ (nextHead g <| s) & enermy .~ (nextenermy g) & enermy1 .~ (nextenermy1 g) & enermy2 .~ (nextenermy2 g)  & enermy3 .~ (nextenermy3 g)  & enermy4 .~ (nextenermy4 g)
move _                             = error "thiefs can't be empty!"

nextHead :: Game -> Coord
nextHead Game { _dir = d, _thief = (a :<| _)}
  | d == North = a & _y %~ (\y -> (y + 1) `mod` height) 
  | d == South = a & _y %~ (\y -> (y - 1) `mod` height)
  | d == East  = a & _x %~ (\x -> (x + 1) `mod` width)
  | d == West  = a & _x %~ (\x -> (x - 1) `mod` width)

nextHead _ = error "thiefs can't be empty!"

nextenermy :: Game -> Coord
nextenermy Game {_enermy = e}
  | e ^. _x >1 && e ^. _y ==4  =  e & _x %~ (\x -> (x - 1) )
  | e ^. _x ==1 && e ^. _y >3  =  e & _y %~ (\y -> (y - 1) )
  | e ^. _x <78 && e ^. _y ==3  =  e & _x %~ (\x -> (x + 1) )
  | e ^. _x ==78 && e ^. _y <4  =  e & _y %~ (\y -> (y + 1) )

nextenermy1 :: Game -> Coord
nextenermy1 Game {_enermy1 = e}
  | e ^. _x >1 && e ^. _y ==37  =  e & _x %~ (\x -> (x - 1) )
  | e ^. _x ==1 && e ^. _y >36  =  e & _y %~ (\y -> (y - 1) )
  | e ^. _x <78 && e ^. _y ==36  =  e & _x %~ (\x -> (x + 1) )
  | e ^. _x ==78 && e ^. _y <37  =  e & _y %~ (\y -> (y + 1) )

nextenermy2 :: Game -> Coord
nextenermy2 Game {_enermy2 = e}
  | e ^. _x >9 && e ^. _y ==25  =  e & _x %~ (\x -> (x - 1) )
  | e ^. _x ==9 && e ^. _y >15  =  e & _y %~ (\y -> (y - 1) )
  | e ^. _x <23 && e ^. _y ==15  =  e & _x %~ (\x -> (x + 1) )
  | e ^. _x ==23 && e ^. _y <25  =  e & _y %~ (\y -> (y + 1) )

nextenermy3 :: Game -> Coord
nextenermy3 Game {_enermy3 = e}
  | e ^. _x >29 && e ^. _y ==25  =  e & _x %~ (\x -> (x - 1) )
  | e ^. _x ==29 && e ^. _y >15  =  e & _y %~ (\y -> (y - 1) )
  | e ^. _x <43 && e ^. _y ==15  =  e & _x %~ (\x -> (x + 1) )
  | e ^. _x ==43 && e ^. _y <25  =  e & _y %~ (\y -> (y + 1) )

nextenermy4 :: Game -> Coord
nextenermy4 Game {_enermy4 = e}
  | e ^. _x >49 && e ^. _y ==25  =  e & _x %~ (\x -> (x - 1) )
  | e ^. _x ==49 && e ^. _y >15  =  e & _y %~ (\y -> (y - 1) )
  | e ^. _x <63 && e ^. _y ==15  =  e & _x %~ (\x -> (x + 1) )
  | e ^. _x ==63 && e ^. _y <25  =  e & _y %~ (\y -> (y + 1) )



turn :: Direction -> Game -> Game
turn d g = if g ^. locked
  then g
  else g & dir %~ turnDir d & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West] && n `elem` [North, South] = n
            | otherwise = c

-- | Draw barry
initba :: (Integral a) => a -> [V2 a]
initba 0 = [V2 0 10]
initba 10 = [V2 10 10] ++ initba(5)
initba 30 = [V2 30 10] ++ initba(20)
initba 50 = [V2 50 10] ++ initba(40)
initba 70 = [V2 70 10] ++ initba(60)
initba n = [V2 n 10] ++ initba(n-1)

initba1 :: (Integral a) => a -> [V2 a]
initba1 0 = [V2 0 30]
initba1 10 = [V2 10 30] ++ initba1(5)
initba1 30 = [V2 30 30] ++ initba1(20)
initba1 50 = [V2 50 30] ++ initba1(40)
initba1 70 = [V2 70 30] ++ initba1(60)
initba1 n = [V2 n 30] ++ initba1(n-1)

initba2 :: (Integral a) => a -> [V2 a]
initba2 5 = [V2 35 5] 
initba2 30 = [V2 35 30] ++ initba2(10)
initba2 n = [V2 35 n] ++ initba2(n-1)

initba3 :: (Integral a) => a -> [V2 a]
initba3 5 = [V2 15 5] 
initba3 30 = [V2 15 30] ++ initba3(10)
initba3 n = [V2 15 n] ++ initba3(n-1)

initba4 :: (Integral a) => a -> [V2 a]
initba4 5 = [V2 55 5] 
initba4 30 = [V2 55 30] ++ initba4(10)
initba4 n = [V2 55 n] ++ initba4(n-1)


initba5 :: (Integral a) => a -> [V2 a]
initba5 15 = [V2 65 15] 
initba5 n = [V2 65 n] ++ initba5(n-1)

initba6 :: (Integral a) => a -> [V2 a]
initba6 15 = [V2 45 15] 
initba6 n = [V2 45 n] ++ initba6(n-1)

initba7 :: (Integral a) => a -> [V2 a]
initba7 15 = [V2 25 15] 
initba7 n = [V2 25 n] ++ initba7(n-1)

initba8 :: (Integral a) => a -> [V2 a]
initba8 15 = [V2 7 15] 
initba8 n = [V2 7 n] ++ initba8(n-1)



initba9 :: (Integral a) => a -> [V2 a]
initba9 0 = [V2 0 0] 
initba9 n = [V2 0 n] ++ initba9(n-1)

initba10 :: (Integral a) => a -> [V2 a]
initba10 0 = [V2 79 0] 
initba10 n = [V2 79 n] ++ initba10(n-1)

initba11 :: (Integral a) => a -> [V2 a]
initba11 0 = [V2 0 0] 
initba11 n = [V2 n 0] ++ initba11(n-1)

initba12 :: (Integral a) => a -> [V2 a]
initba12 0 = [V2 0 39] 
initba12 n = [V2 n 39] ++ initba12(n-1)

-- For this game, we have fixed golds and barry locations.
initGame :: IO Game
initGame = do
  let xm = width `div` 2
      ym = height `div` 2
      g  = Game
        { _thief  = (S.singleton (V2 xm ym))
        , _gold   = (V2 22 42)
        , _enermy = (V2 1 3) 
        , _enermy1 = (V2 78 37) 
        , _enermy2 = (V2 10 25) 
        , _enermy3 = (V2 30 25) 
        , _enermy4 = (V2 50 25) 
        , _golds  = (S.fromList ([(V2 17 20),(V2 35 20),(V2 55 20),(V2 3 3),(V2 77 37),(V2 15 20),(V2 33 20),(V2 53 20),(V2 77 3),(V2 3 37)]))
        , _barry  = (S.fromList (initba(80) ++ initba1(80) ++ initba2(35)++ initba3(35)++ initba4(35)++ initba5(25)++ initba6(25)++ initba7(25)++ initba8(25)++ initba9(40)++ initba10(40)++ initba11(79)++ initba12(79)))
        , _score  = 0
        , _dir    = North
        , _dead   = False
        , _win   = False
        , _paused = True
        , _locked = False
        }
  
  return $ execState nextgold g

fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")