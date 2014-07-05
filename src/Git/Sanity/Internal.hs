module Git.Sanity.Internal where

import Data.Machine

-- TODO Contribute to machine?
slide :: Process a (a, a)
slide = f Nothing where
  f Nothing   = await (\x -> f $ Just x)
  f (Just x)  = await (\y -> machine $ Yield (x, y) $ f $ Just y)
  await f = machine $ Await f Refl stopped
  machine = MachineT . return
