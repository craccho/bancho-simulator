module Bancho where

import Control.Monad.State
import System.Random

data Combination =
  WeakCherry |
  StrongCherry |
  StrongestCherry |
  LeftBell |
  CenterBell |
  RightBell |
  CommonBell |
  WeakBento |
  StrongBento |
  ChanceA |
  ChanceB |
  ChanceC |
  Replay |
  SuperBonus |
  Blank
  deriving (Show, Eq, Enum, Bounded)

allButBlank :: [Combination]
allButBlank = filter (/= Blank) allCombi

allCombi :: [Combination]
allCombi = enumFrom minBound

data Mode =
  Reset |
  NormalA |
  NormalB |
  HighA |
  HighB
  deriving Show

data ReplayMode =
  RNormal |
  RPrepare |
  RBonus |
  RRush
  deriving Show

data Regulation =
  S1 | S2 | S3 | S4 | S5 | S6
  deriving (Eq, Ord)

instance Show Regulation where
  show S1 = "設定1"
  show S2 = "設定2"
  show S3 = "設定3"
  show S4 = "設定4"
  show S5 = "設定5"
  show S6 = "設定6"

data SlotState = SlotState {
  regulation :: Regulation ,
  totalPlayCount :: Int ,
  partialPlayCount :: Int ,
  medals :: Int ,
  mode :: Mode ,
  replayMode :: ReplayMode ,
  gen :: StdGen
} deriving Show

type Probability = Double

probability :: Regulation -> ReplayMode -> Combination -> Probability
probability s rm c = case c of
  WeakCherry -> 1/119.16
  StrongCherry -> 1/436.91
  StrongestCherry -> 1/16384.0
  LeftBell -> case s of
    S1 -> 1/163.84
    S2 -> 1/156.04
    S3 -> 1/148.95
    S4 -> 1/142.47
    S5 -> 1/136.53
    S6 -> 1/128.00
  CenterBell -> 1/6.69
  RightBell -> 1/6.69
  CommonBell -> case s of
    S1 -> 1/74.14
    S2 -> 1/75.85
    S3 -> 1/77.65
    S4 -> 1/79.53
    S5 -> 1/81.51
    S6 -> 1/84.89
  WeakBento -> 1/85.11
  StrongBento -> 1/32768.0
  SuperBonus -> 1/32768.0
  ChanceA -> 1/399.61
  ChanceB -> 1/399.61
  ChanceC -> 1/399.61
  Replay -> case rm of
    RNormal -> 1/7.3
    otherwise -> 1/1.55
  Blank -> 1 - sum (map (probability s rm) allButBlank)

combiTable :: Regulation -> ReplayMode -> [(Probability, Combination)]
combiTable reg rm =
  reverse $ foldl (\rs c ->
    let r = head rs in [(fst r + probability reg rm c, c)] ++ rs
  ) [(probability reg rm Blank, Blank)] allButBlank

combiPick :: Regulation -> ReplayMode -> Probability -> Combination
combiPick reg rm p =
  let ct = combiTable reg rm
      pick p (pc:sct) = if p < fst pc then snd pc else pick p sct
      pick p [] = Blank
  in pick p ct

data PushCombi = LCR | LRC | CLR | CRL | RLC | RCL
  deriving (Show, Eq)

data Strategy = LeftBar | ArtFull deriving Show

data Push = Push {
  pc :: PushCombi,
  luck :: Probability
} deriving Show

pay :: Strategy -> Combination -> State SlotState Int
pay strt c = do
  s <- get
  let l :: Probability
      (l, g) = randomR (0.0, 1.0) (gen s)
      pc = case (replayMode s) of
        RNormal -> LCR
        RPrepare -> correct
        RBonus -> correct
        RRush -> correct
        where
        correct = case c of
          LeftBell -> LCR
          CenterBell -> CLR
          RightBell -> RLC
          otherwise -> LCR
  put $ s {gen = g}
  return $ case c of
    WeakCherry -> 2
    StrongCherry -> 2
    StrongestCherry -> 2
    LeftBell | pc == LCR || pc == LRC || l < 1/4 -> 9
             | otherwise -> 0
    CenterBell | pc == CLR || pc == CRL || l < 1/4 -> 9
               | otherwise -> 0
    RightBell | pc == RLC || pc == RCL || l < 1/4 -> 9
              | otherwise -> 0
    CommonBell -> 9
    WeakBento -> 5
    StrongBento -> 5
    ChanceA -> 9
    ChanceB -> 9
    ChanceC -> case strt of
      LeftBar -> 0
      ArtFull -> 1
    Replay -> 3
    SuperBonus -> 0
    Blank -> 0

play :: State SlotState (Combination, Int)
play = do
  s <- get
  let (p, g) = randomR (0.0, 1.0) (gen s)
      c = combiPick (regulation s) (replayMode s) p
  put $ s {gen = g}
  payout <- pay ArtFull c
  s <- get
  let s' = s {medals = (medals s) + payout - 3,
              totalPlayCount = (totalPlayCount s) + 1,
              partialPlayCount = (partialPlayCount s) + 1
              }
  put s'
  return (c, payout)

playUntil :: Int -> State SlotState Int
playUntil m = do
  play
  s <- get
  if medals s > m then playUntil m else return $ totalPlayCount s

initialState :: IO SlotState
initialState = do
  g <- getStdGen
  return $ SlotState S1 0 0 0 Reset RBonus g

simulate :: Int -> IO ([(Combination, Int)], SlotState)
simulate g = do
  s <- initialState
  let log = (`runState` s) $ sequence $ replicate g play
  return $ log

main = do
  log <- simulate 1000
  print log
