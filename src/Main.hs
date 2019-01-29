module Main where

import           Test.QuickCheck
import           Test.QuickCheck.Monadic

import           BrbFFI
import           Tests

prop_trackingUsedSpace =
  monadicIO $ do
    run $ c_setUp 1000
    l <- run $ generate $ choose (10, 1000)
    s <- run $ generate (vectorOf l $ choose (0, 255)) >>= offer
    assert $ s == fromIntegral l
    _ <- run $ poll (fromIntegral s `div` 2)
    s' <- run used
    assert $ s' == (fromIntegral l - fromIntegral s `div` 2)
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
