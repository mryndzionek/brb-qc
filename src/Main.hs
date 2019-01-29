module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           BrbFFI
import           Tests

prop_trackingUsedSpace =
  monadicIO $ do
    run $ c_setUp 1000
    s <- run $ generate (vectorOf 100 $ choose (0, 255)) >>= offer
    assert $ s == 100
    _ <- run $ poll 50
    s' <- run used
    assert $ s' == 50
    run c_tearDown

prop_offerThenPoll (OfferThenPoll cs ops) =
  monadicIO $ do
    run $ c_setUp $ fromIntegral $ length cs
    out <- run $ transaction ops
    assert $ out == cs
    run c_tearDown

prop_generalCorrectTest mixed =
  monadicIO $ do
    ops <- run $ getOps mixed
    let offs = getOffers ops
        len = fromIntegral $ length offs
    run $ c_setUp len
    out <- run $ transaction ops
    assert $ out == offs
    len' <- run used
    assert $ len' == 0
    run c_tearDown

deepCheck :: Testable prop => prop -> IO ()
deepCheck = quickCheckWith (stdArgs {maxSuccess = 1000})

main :: IO ()
main = do
  deepCheck prop_trackingUsedSpace
  deepCheck prop_offerThenPoll
  deepCheck prop_generalCorrectTest
