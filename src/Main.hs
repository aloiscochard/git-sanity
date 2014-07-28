module Main where

import Distribution.Text (display)
import Paths_git_sanity (version)
import System.Environment
import System.Exit

import Git.Sanity (analyze)

name :: String
name = "git-sanity"

help :: IO ()
help = putStrLn $
  unlines [ concat ["Usage: ", name, " [check [<revision range>]]" ]
          , "                  [--help]"
          , "                  [--version]"
          , ""
          , " <revision range>  Check only commits in the specified revision range."
          , ""
          , "When no <revision range> is specified, it defaults to HEAD (i.e. the whole history leading to the current commit)."
          , ""
          , "In order to integrate nicely as a pre-push githooks (http://git-scm.com/docs/githooks.html),"
          , "a <revision range> of 'origin/$branch~1..HEAD'[1] can be used."
          , ""
          , "[1] Where 'branch' is a variable defined as 'branch=$(git symbolic-ref --short HEAD)'" ]

main :: IO ()
main = do
  args  <- getArgs
  run args where
    check range = do
      (exitCode, total) <- analyze range
      if total == 0 then
        exitWith exitCode
      else do
        putStrLn ""
        putStrLn $ concat [name, ": ", show total, " insane commit(s)."]
        exitFailure
    run ["check", range]  = check range
    run ["check"]         = check "HEAD"
    run ["--version"]     = putStrLn $ concat [name, ": ", display version]
    run ["--help"]        = help
    run []                = help
    run (x:_)             = do
      putStrLn $ concat [name, ": '", x,"' is not a valid command. See '", name, " --help'."]
      exitWith (ExitFailure 1)
