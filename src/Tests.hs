module Tests where

import Data.List.Split
import Foreign.C.Types
import Test.QuickCheck

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

getOps :: [Offer] -> [Offer] -> [Poll] -> [Poll] -> Gen [Operation]
getOps pre offers polls post = do
  ops <- shuffle $ map Oo offers ++ map Op polls
  return $ map Oo pre ++ ops ++ map Op post

mkMixedOperations :: Int -> Int -> Gen MixedOperations
mkMixedOperations preC offersC = do
  pre <- gOffers preC
  post <- gPolls offersC
  offers <- gOffers offersC
  polls <- gPolls preC
  ops <- getOps pre offers polls post
  return $
    MixedOperations
      (length pre)
      (length offers + length polls)
      (length post)
      ops

data MixedOperations = MixedOperations
  { _pre :: Int
  , _mid :: Int
  , _post :: Int
  , _ops :: [Operation]
  } deriving (Show)

instance Arbitrary MixedOperations where
  arbitrary = sized $ \n -> do
    let (s, e) = (1, n + 1)
    preCount <- choose (s, e)
    oCount <- choose (s, e)
    mkMixedOperations preCount oCount
  shrink mixed =
    let (pr, mid, po) = (_pre mixed, _mid mixed, _post mixed)
        (pr':mid':po') = splitPlaces [pr, mid, po] $ _ops mixed
        crit m = _pre m > 0 && _mid m > 0 && _post m > 0
     in filter
          crit
          [ MixedOperations (pr - 1) mid po (init pr' ++ mid' ++ head po')
          , MixedOperations pr (mid - 1) po (pr' ++ init mid' ++ head po')
          , MixedOperations pr mid (po - 1) (pr' ++ mid' ++ (init . head) po')
          ]
