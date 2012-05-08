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
  deriving (Show, Eq)

allButBlank :: [Combination]
allButBlank = [
  WeakCherry, StrongCherry, StrongestCherry, LeftBell,
  CenterBell, RightBell, CommonBell, WeakBento, StrongBento,
  Replay, SuperBonus, ChanceA, ChanceB, ChanceC ]

allCombi :: [Combination]
allCombi = Blank : allButBlank

  
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

data Strategy = LeftBar | ArtFull

data Push = Push {
  pc :: PushCombi,
  strategy :: Strategy,
  luck :: Probability
}

pay :: Push -> Combination -> Int
pay p c = case c of
  WeakCherry -> 2
  StrongCherry -> 2
  StrongestCherry -> 2
  LeftBell | pc p == LCR || pc p == LRC || luck p < 1/4 -> 9
           | otherwise -> 0
  CenterBell | pc p == CLR || pc p == CRL || luck p < 1/4 -> 9
             | otherwise -> 0
  RightBell | pc p == RLC || pc p == RCL || luck p < 1/4 -> 9
            | otherwise -> 0
  CommonBell -> 9
  WeakBento -> 5
  StrongBento -> 5
  ChanceA -> 9
  ChanceB -> 9
  ChanceC -> case strategy p of
    LeftBar -> 0
    ArtFull -> 1
  Replay -> 3
  SuperBonus -> 0
  Blank -> 0

play :: State SlotState (Combination, SlotState)
play = do
  s <- get
  let (p, g) = randomR (0.0, 1.0) (gen s)
  let (p', g') = randomR (0.0, 1.0) g
  let c = combiPick (regulation s) (replayMode s) p
  let payout = pay (Push LCR LeftBar p') c
  let s' = s {medals = (medals s) + payout - 3,
              totalPlayCount = (totalPlayCount s) + 1,
              partialPlayCount = (partialPlayCount s) + 1,
              gen = g'}
  put s'
  return (c, s')

playUntil :: Int -> State SlotState Int
playUntil m = do
  play
  s <- get
  if medals s > m then playUntil m else return $ totalPlayCount s

initialState :: IO SlotState
initialState = do
  g <- getStdGen
  return $ SlotState S1 0 0 0 Reset RNormal g

main = do
  s <- initialState
  putStrLn $ show s
  putStrLn $ show $ probability S1 RNormal Blank
  let log = (`runState` s) $ sequence $ take 1000 $ repeat play
  putStrLn $ show log
