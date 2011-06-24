module Main where

import Test.Framework.Providers.DocTest
import Test.Framework

main = docTest ["src/Test/Framework/Providers/DocTest.hs"] [] >>= defaultMain . return