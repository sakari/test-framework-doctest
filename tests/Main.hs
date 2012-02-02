module Main where

import Test.Framework.Providers.DocTest
import Test.Framework

main = docTest ["src/Test/Framework/Providers/DocTest.hs"] ["-optP-include", "-optPdist/build/autogen/cabal_macros.h"] >>= defaultMain . return
