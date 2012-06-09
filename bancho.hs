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

data BBType =
  RedBB |
  BlueBB |
  Regular
  deriving (Show)

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
  limitGameCount :: Int ,
  bbPlayCount :: Int ,
  bbType :: BBType ,
  medals :: Int ,
  mode :: Mode ,
  replayMode :: ReplayMode ,
  gen :: StdGen
} deriving Show

defaultSlotState :: SlotState
defaultSlotState = SlotState {
  regulation = S1,
  totalPlayCount = 0,
  partialPlayCount = 0,
  limitGameCount = 600,
  bbPlayCount = 0,
  bbType = RedBB,
  medals = 0,
  mode = Reset,
  replayMode = RNormal,
  gen = undefined
  }

type Probability = Double

getBB :: State SlotState (BBType, Int)
getBB = do
  s <- get
  let table = case mode s of
        Reset ->
          bbt 0.26 0.732 0.008
        NormalA -> case regulation s of
          S1 -> bbt 0.500 0.485 0.016
          S2 -> bbt 0.530 0.454 0.016
          S3 -> bbt 0.500 0.485 0.016
          S4 -> bbt 0.530 0.454 0.016
          S5 -> bbt 0.484 0.500 0.016
          S6 -> bbt 0.435 0.549 0.016
      bbt rb bbr bbb = [(Regular, rb), (RedBB, bbr), (BlueBB, bbb)]
  bbType <- feedProb $ select table
  bbLimit <- case bbType of
        Regular -> return 30
        otherwise -> do
          let gt = case regulation s of
                S1 -> s13
                S2 -> s13
                S3 -> s13
                S4 -> s45
                S5 -> s45
                S6 -> s6
              s13 = bt 49.10 44.59 4.25 1.40    0 0.14 0.04 0.04 0.39
              s45 = bt 49.19 44.61 4.18 1.22    0 0.29 0.04 0.04 0.39
              s6  = bt 44.79 48.91 4.32 1.21 0.39 0.19 0.03 0.03 0.09
              bt = \g60 g90 g120 g150 g180 g210 g240 g270 g300 -> map (\(a, b) -> (a, b / 10))
                [(60, g60), (90, g90), (120, g120), (150, g150), (180, g180), (210, g210),
                 (240, g240), (270, g270), (300, g300)]
          feedProb $ select gt
  return (bbType, bbLimit)

select :: [(a, Probability)] -> Probability -> a
select ((some, _):[]) _ = some
select ((some, c):ps) p = if p < c then some else select ps (p - c)

getNormalLimit :: State SlotState Int
getNormalLimit = return 50 -- TODO: select from Table

processPartialCount :: State SlotState Int
processPartialCount = do
  s <- get
  let pp = partialPlayCount s
      lg = limitGameCount s
  case replayMode s of
    RNormal -> do
      if pp > lg then do
        (bbType, newLimit) <- getBB
        put $ s {
          partialPlayCount = 0 ,
          limitGameCount = newLimit ,
          replayMode = RBonus ,
          bbType = bbType
          }
        return newLimit
      else do
        put $ s {
          partialPlayCount = pp + 1
          }
        return lg

    RBonus -> do
      if pp > lg then do
        newLimit <- getNormalLimit
        put $ s {
          partialPlayCount = 0 ,
          limitGameCount = newLimit ,
          replayMode = RNormal
          }
        return newLimit
      else do
        put $ s {
          partialPlayCount = pp + 1
          }
        return lg

    otherwise -> do
      put $ s {
        partialPlayCount = pp + 1
        }
      return lg

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

feedProb :: (Probability -> a) -> State SlotState a
feedProb f = do
  s <- get
  let (p, g) = randomR (0.0, 1.0) (gen s)
  put $ s {gen = g}
  return $ f p

play :: State SlotState (Combination, Int)
play = do
  s <- get
  c <- feedProb $ combiPick (regulation s) (replayMode s)
  payout <- pay ArtFull c
  modify $ \s -> s {medals = (medals s) + payout - 3,
                    totalPlayCount = (totalPlayCount s) + 1,
                    partialPlayCount = (partialPlayCount s) + 1
  }
  processPartialCount
  return (c, payout)

playUntil :: Int -> State SlotState Int
playUntil m = do
  play
  s <- get
  if medals s > m then playUntil m else return $ totalPlayCount s

initialState :: IO SlotState
initialState = do
  g <- getStdGen
  return $ defaultSlotState { replayMode = RNormal, gen = g }

simulate :: Int -> IO ([(Combination, Int)], SlotState)
simulate g = do
  s <- initialState
  let log = (`runState` s) $ sequence $ replicate g play
  return $ log

main = do
  newStdGen
  log <- simulate 7000
  print log
