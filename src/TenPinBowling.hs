{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module TenPinBowling
  ( Roll (Roll),
    Score (Score),
    score,
  )
where

newtype Roll = Roll Int deriving (Show, Eq, Ord, Num)

newtype Score = Score Int deriving (Show, Eq, Ord, Num)

data Frame
  = Standard Roll Roll
  | Spare Roll Roll Roll
  | Strike Roll Roll
  deriving (Show, Eq)

frameScore :: Frame -> Score
frameScore (Standard (Roll r1) (Roll r2)) = Score $ r1 + r2
frameScore (Spare (Roll r1) (Roll r2) (Roll b)) = Score $ r1 + r2 + b
frameScore (Strike (Roll b1) (Roll b2)) = Score $ 10 + b1 + b2

toFrames :: [Roll] -> Maybe [Frame]
toFrames = toFrames' 1

toFrames' :: Int -> [Roll] -> Maybe [Frame]
toFrames' 11 _ = Just []
toFrames' frameNo (x1 : x2 : xs)
  | x1 + x2 < 10 = fmap (Standard x1 x2 :) (toFrames' nextFrame xs)
  | x1 + x2 == 10 = fmap (Spare x1 x2 nextRoll :) (toFrames' nextFrame xs)
  | x1 == 10 = fmap (Strike x2 nextRoll :) (toFrames' nextFrame (x2 : xs))
  where
    nextRoll = head xs
    nextFrame = frameNo + 1
toFrames' _ _ = Nothing

score :: [Roll] -> Maybe Score
score xs = do
  frames <- toFrames xs
  return $ foldr ((+) . frameScore) 0 frames
