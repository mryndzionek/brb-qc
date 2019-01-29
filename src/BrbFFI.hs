module BrbFFI
  ( c_setUp
  , offer
  , poll
  , used
  , transaction
  , c_tearDown
  ) where

import           Foreign.C.Types
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable

import           Tests

foreign import ccall "rbiSetUp" c_setUp :: CSize -> IO ()

foreign import ccall "rbiOffer" c_offer :: Ptr CUChar -> CSize -> IO CInt

foreign import ccall "rbiPoll" c_poll :: CSize -> IO (Ptr CUChar)

foreign import ccall "rbiUsed" c_used :: IO CInt

foreign import ccall "rbiTearDown" c_tearDown :: IO ()

offer :: [CUChar] -> IO CInt
offer cs = do
  let l = length cs
  p <- mallocBytes l
  mapM_ (uncurry $ pokeElemOff p) $ zip [0 .. l - 1] cs
  s <- c_offer p (fromIntegral $ length cs)
  free p
  return s

poll :: CSize -> IO [CUChar]
poll s = do
  let l = fromIntegral s
  p <- c_poll s
  if p == nullPtr
    then pure []
    else mapM (peekElemOff p) [0 .. l - 1]

used :: IO CInt
used = c_used

transaction :: [Operation] -> IO [CUChar]
transaction ops = do
  res <- proc ops :: IO [CUChar]
  u <- used
  -- we don't know for how many bytes we need to poll
  -- in order to empty the buffer, try all possibilities
  fin <- proc $ concat $ replicate 2 [Op $ fromIntegral x | x <- [u,u - 1 .. 1]]
  return $ res ++ fin
  where
    proc o = concat <$> mapM run o
    run (Oo o) = offer o >> pure []
    run (Op p) = poll p
