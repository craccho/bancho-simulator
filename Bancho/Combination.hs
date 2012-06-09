module Bancho.Combination where

data Combination =
  WeakCherry | StrongCherry | StrongestCherry |
  LeftBell | CenterBell | RightBell | CommonBell |
  WeakBento | StrongBento |
  ChanceA | ChanceB | ChanceC |
  Replay |
  SuperBonus |
  Blank
  deriving (Show, Eq, Enum, Bounded, Ord)

allButBlank :: [Combination]
allButBlank = filter (/= Blank) allCombi

allCombi :: [Combination]
allCombi = enumFrom minBound

