{-# LANGUAGE Rank2Types #-}
module Git.Sanity where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Data.ByteString.Char8 (ByteString)
import Data.Machine
import Safe
import System.Exit (ExitCode)
import System.IO.Machine (IODataMode(..), byLine)
import System.Process (CreateProcess(..), StdStream(CreatePipe), shell)
import System.Process.Machine (callProcessMachines, mStdOut)

import qualified Data.ByteString.Char8 as BS

import Git.Sanity.Internal

type Range = String
type Line = ByteString
type Hash = ByteString

analyze :: Range -> IO (ExitCode, Int)
analyze range = do
  res <- callProcessMachines byLine (gitLogParents range) (mStdOut $ report <~ filterInsane <~ slide <~ parseHashes)
  return $ length <$> res

gitLogParents :: Range -> CreateProcess
gitLogParents range = (shell $ concat ["git log ", range, " --parents | cat"]) { std_out = CreatePipe }

-- | Parse `git log --parents` command output and return a stream of commit parents hashes
parseHashes :: Process Line [Hash]
parseHashes = fmap parse $ filtered (BS.isPrefixOf prefix) where
  parse = (BS.split ' ') . BS.drop (BS.length prefix)
  prefix = BS.pack "commit "

filterInsane :: Process ([Hash], [Hash]) (Hash)
filterInsane = repeatedly f where
  f = await >>= \(xs, ys) -> if last xs == head ys then f else yield $ head xs

report :: ProcessT IO Hash ()
report = repeatedly $ do
  x <- await
  liftIO . putStrLn $ BS.unpack x
  yield ()
