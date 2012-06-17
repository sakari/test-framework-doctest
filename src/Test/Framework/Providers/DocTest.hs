{-# OPTIONS_HADDOCK prune #-}
-- |
-- This package is deprecated, use Doctest`s cabal integration instead.
--
-- See: <https://github.com/sol/doctest-haskell#cabal-integration>
module Test.Framework.Providers.DocTest (docTest) where

import qualified Test.DocTest as DocTest
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.List(groupBy)
import Data.Monoid

noColors :: RunnerOptions
noColors =  mempty {
    ropt_color_mode = Nothing
}

{-# DEPRECATED docTest "Use Doctest's cabal integration instead!" #-}
docTest::[FilePath] -> [String] -> IO Test
docTest rootPaths options = do
  tests <- DocTest.getDocTests options rootPaths
  return $ toTestFrameworkGroup (rootPaths ++ options) tests

toTestFrameworkTest :: [String] -> DocTest.DocTest -> Test
toTestFrameworkTest options test = testCase testName $ DocTest.withInterpreter options $ flip DocTest.toAssertion test
  where
    testName = DocTest.firstExpression test

toTestFrameworkGroup :: [String] -> [DocTest.DocTest] -> Test
toTestFrameworkGroup options examples = testGroup "DocTest" $ map fileTestGroup $ groupBy w examples
  where
    w left right = DocTest.sourcePath left == DocTest.sourcePath right
    fileTestGroup examples = testGroup fileName $ toTestFrameworkTest options `map` examples
      where
        fileName = DocTest.sourcePath $ head $ examples
