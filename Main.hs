module Main where

import Data.Foldable
import Data.Traversable
import Data.List
import System.Process

data Dependency = Dependency
                { name    :: String
                , version :: String
                } deriving (Eq, Ord, Show)

parseDependency :: [String] -> [Dependency]
parseDependency [depName, "==", depVersion] = [Dependency depName depVersion]
parseDependency _                           = []

runCabal2Nix :: Dependency -> IO ()
runCabal2Nix (Dependency depName depVersion) = do
  readProcess "cabal2nix" ["--hackage-db", "/home/sean/.cabal/packages/hackage.haskell.org/01-index.tar", "cabal://" ++ depName ++ "-" ++ depVersion] ""
  return ()

filterSpecialDependencies :: Dependency -> Bool
filterSpecialDependencies (Dependency "base" _)         = False
filterSpecialDependencies (Dependency "ghc-prim" _)     = False
filterSpecialDependencies (Dependency "rts" _)          = False
filterSpecialDependencies (Dependency "integer-gmp" _)  = False
filterSpecialDependencies (Dependency "deepseq" _)      = False
filterSpecialDependencies (Dependency "array" _)        = False
filterSpecialDependencies (Dependency "directory" _)    = False
filterSpecialDependencies (Dependency "filepath" _)     = False
filterSpecialDependencies (Dependency "time" _)         = False
filterSpecialDependencies (Dependency "unix" _)         = False
filterSpecialDependencies (Dependency "bytestring" _)   = False
filterSpecialDependencies _                             = True

main :: IO ()
main = do
  -- Run cabal freeze --dry-run
  cabalFreezeOutput <- readProcess "cabal" ["freeze", "--dry-run"] ""

  -- Grab the entries from the output.
  let dependencies = lines cabalFreezeOutput >>= (\line -> parseDependency $ words line)

  -- For each entry invoke cabal2nix.
  traverse_ runCabal2Nix $ sort $ filter filterSpecialDependencies dependencies