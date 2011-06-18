module Main where

import Test.Framework.DocTest
import Test.Framework

main = docTest ["src/Test/Framework/DocTest.hs"] [] >>= defaultMain . return