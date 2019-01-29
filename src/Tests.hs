module Tests where

import           Data.Function
import           Data.List.Split
import           Foreign.C.Types
import           Test.QuickCheck

type Offer = [CUChar]

type Poll = CSize

data Operation
  = Op Poll
  | Oo Offer
  deriving (Show)

data OfferThenPoll =
  OfferThenPoll [CUChar]
                [Operation]
  deriving (Show)

instance Arbitrary OfferThenPoll where
  arbitrary = do
    offers <- gOffers 200
    polls <- gPolls 200
    return $ OfferThenPoll (concat offers) $ map Oo offers ++ map Op polls

gOffers :: Int -> Gen [Offer]
gOffers s = do
  parts <- infiniteListOf $ choose (1, s `div` 2)
  content <- vectorOf s $ choose (0, 255)
  return $ splitPlaces parts content

gPolls :: Int -> Gen [Poll]
gPolls s = do
  parts <- infiniteListOf $ choose (1, s `div` 2)
  return $ map sum $ splitPlaces parts $ replicate s 1

getOffers :: [Operation] -> Offer
getOffers = concatMap strip
  where
    strip (Oo o) = o
    strip (Op _) = []

getOps :: MixedOperations -> IO [Operation]
getOps mixed = do
  ops <- generate $ shuffle $ map Oo (_offers mixed) ++ map Op (_polls mixed)
  return $ map Oo (_pre mixed) ++ ops ++ map Op (_post mixed)

data MixedOperations = MixedOperations
  { _pre    :: [Offer]
  , _offers :: [Offer]
  , _polls  :: [Poll]
  , _post   :: [Poll]
  } deriving (Show)

instance Arbitrary MixedOperations where
  arbitrary = do
    preCount <- growingElements [10 .. 1000]
    oCount <- growingElements [10 .. 1000]
    pre <- gOffers preCount
    post <- gPolls oCount
    offers <- gOffers oCount
    polls <- gPolls preCount
    return $ MixedOperations pre offers polls post
  shrink mixed =
    let pre = _pre mixed
        off = _offers mixed
        pol = _polls mixed
        pos = _post mixed
        nulls m =
          not . or $
          fmap (m &) [null . _pre, null . _offers, null . _polls, null . _post]
     in filter
          nulls
          [ MixedOperations (init pre) off pol pos
          , MixedOperations pre (init off) pol pos
          , MixedOperations pre off (init pol) pos
          , MixedOperations pre off pol (init pos)
          ]
