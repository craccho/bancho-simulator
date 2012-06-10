module Bancho where

import Control.Monad.State
import System.Random

import Bancho.Combination

data Mode =
  Reset |
  NormalA | NormalB |
  HighA | HighB
  deriving (Show)

data ReplayMode =
  RNormal |
  RPrepare |
  RBonus |
  RRush
  deriving (Show)

data BBType =
  RedBB |
  BlueBB |
  Regular
  deriving (Show)

data Regulation =
  S1 | S2 | S3 | S4 | S5 | S6
  deriving (Eq, Ord)

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
} deriving (Show)

data PushCombi = LCR | LRC | CLR | CRL | RLC | RCL
  deriving (Show, Eq)

data Strategy = LeftBar | ArtFull deriving (Show)

data Push = Push {
  pc :: PushCombi,
  luck :: Probability
} deriving (Show)


type GameState = State SlotState

type Probability = Double

instance Show Regulation where
  show S1 = "設定1"
  show S2 = "設定2"
  show S3 = "設定3"
  show S4 = "設定4"
  show S5 = "設定5"
  show S6 = "設定6"


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

getBB :: GameState (BBType, Int)
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
select ps p = let psum = sum . map snd $ ps
                  p' = p * psum
              in select' ps p'
  where
    select' ((some, _):[]) _ = some
    select' ((some, c):ps) p = if p < c then some else select' ps (p - c)

getNormalLimit :: GameState Int
getNormalLimit = do
  s <- get
  let m = mode s
      reg = regulation s
      table = case m of
        Reset -> ta
        NormalA -> ta
        NormalB -> case reg of
          S1 -> gt 0.40 0.40 0.79 1.93 10.03 4.31 15.28 3.33 19.20 2.21 12.73 1.47 8.32 0.04 19.27
          S2 -> gt 0.41 0.41 0.81 1.95 10.51 4.50 15.86 3.43 19.58 2.23 12.71 1.45 8.13 0.04 17.73
          S3 -> gt 0.40 0.40 0.79 1.93 10.03 4.31 15.28 3.33 19.20 2.21 12.73 1.47 8.32 0.04 19.27
          S4 -> gt 0.42 0.42 0.84 1.98 10.64 4.59 15.93 3.49 19.56 2.26 12.64 1.46 8.05 0.04 17.43
          S5 -> gt 0.40 0.40 0.79 1.93 10.03 4.31 15.28 3.33 19.20 2.21 12.73 1.47 8.32 0.04 19.27
          S6 -> gt 0.79 0.79 1.55 2.65 13.86 6.56 17.92 4.56 19.24 2.62 11.05 1.50 6.15 0.06 10.71
        HighA -> gtt 10 20 50 20
        HighB -> gtt 8.45 18.31 36.62 36.62
      ta = case reg of
        S1 -> gt 1.20 2.66 5.16 2.15 1.04 26.42 0.69 18.26 0.48 12.62 0.33 8.72 0.12 17.94 2.19
        S2 -> gt 1.21 2.67 5.18 2.16 1.09 27.50 0.71 18.65 0.48 12.65 0.33 8.58 0.11 16.63 2.03
        S3 -> gt 1.20 2.66 5.16 2.15 1.04 26.42 0.69 18.26 0.48 12.62 0.33 8.72 0.12 17.94 2.19
        S4 -> gt 1.67 3.76 5.78 4.24 1.06 24.98 0.71 17.29 0.49 11.97 0.34 8.29 0.12 16.08 3.22
        S5 -> gt 1.64 3.73 5.74 4.21 0.94 23.90 0.64 16.90 0.45 11.95 0.32 8.44 0.12 17.43 3.59
        S6 -> gt 2.03 4.09 6.35 4.73 2.70 26.14 1.63 17.68 1.02 11.11 0.64 6.98 0.21 10.48 4.20
      gt r16 r32 r64 r96 r200 r300 r400 r500 r600 r700 r800 r900 r951 r967 r999 =
        [([1..16], r16), ([17..32], r32), ([33..64], r64), ([65..96], r96), ([97..200], r200),
         ([201..300], r300), ([301..400], r400), ([401..500], r500), ([501..600], r600),
         ([601..700], r700), ([701..800], r800), ([801..900], r900),
         ([901..951], r951), ([952..967], r967), ([968..999], r999)]
      gtt r16 r32 r64 r96 = [([1..16], r16), ([17..32], r32), ([33..64], r64), ([65..96], r96)]
  ptable <- feedProb $ select table
  let rtable = case length ptable of
        100 -> zip [head ptable ..] $ (replicate 32 0) ++ (replicate 32 (1 / 32)) ++ (replicate 36 0)
        l -> zip [head ptable ..] (replicate l (1 / fromIntegral l))
  feedProb $ select rtable

processPartialCount :: GameState Int
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

combiTable :: Regulation -> ReplayMode -> [(Combination, Probability)]
combiTable reg rm = map (\c -> (c, probability reg rm c)) allCombi

combiPick :: Regulation -> ReplayMode -> Probability -> Combination
combiPick reg rm p = select (combiTable reg rm) p

pay :: Strategy -> Combination -> GameState Int
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

feedProb :: (Probability -> a) -> GameState a
feedProb f = do
  s <- get
  let (p, g) = randomR (0.0, 1.0) (gen s)
  put $ s {gen = g}
  return $ f p

play :: GameState (Combination, Int)
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

playUntil :: Int -> GameState Int
playUntil m = do
  play
  s <- get
  if medals s > m then playUntil m else return $ totalPlayCount s

initialState :: Regulation -> IO SlotState
initialState reg = do
  g <- getStdGen
  return $ defaultSlotState { replayMode = RNormal, gen = g, regulation = reg }

simulate :: Regulation -> Int -> IO ([(Combination, Int)], SlotState)
simulate reg g = do
  s <- initialState reg
  let log = (`runState` s) $ sequence $ replicate g play
  return $ log

main = do
  newStdGen
  log <- simulate S1 7000
  print log
