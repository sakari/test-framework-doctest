module Main where

import Test.Framework.DocTest
import Test.Framework

main = frameDocTestsFrom "src/Test/Framework/DocTest.hs" [] >>= defaultMain . return