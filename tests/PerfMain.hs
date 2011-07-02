module Main where

import Test.Framework.Providers.DocTest
import Test.Framework

main = docTest ["tests/Performance.hs"] [] >>= defaultMain . return