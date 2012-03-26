{-# LANGUAGE CPP #-}

{-| Wrapper for running DocTests with Test.Framework

First we get the doctests wrapped in 'Test.Framework.Test' using
'docTest'.  The first argument to 'docTest' should be the root modules
i.e., the modules that are not imported by other modules.

>>> doctests <- docTest ["tests/Test.hs"] ["-itests"]

After getting the doctests we can execute the doctests using the
'defaultMain' or 'defaultMainWithOpts' functions.

>>> :m + Data.Monoid
>>> defaultMainWithOpts [doctests] noColors
DocTest:
  tests/Test.hs:
    print "abc": [Failed]
expression `print "abc"'
expected: ["\"fail\""]
 but got: ["\"abc\""]
    print bar: [OK]
<BLANKLINE>
         Test Cases  Total      
 Passed  1           1          
 Failed  1           1          
 Total   2           2          
*** Exception: ExitFailure 1

The @*** Exception: ExitFailure 1@ is caused by
'defaultMainWithOptions' trying to exit after finishing with tests.

-}

module Test.Framework.Providers.DocTest (docTest) where

import Documentation.Haddock
import qualified Test.DocTest as DocTest
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.List(groupBy)
import Data.Monoid

noColors :: RunnerOptions
noColors =  mempty {
#if MIN_VERSION_test_framework(0,5,0)
    ropt_color_mode = Nothing
#else
    ropt_plain_output = Just True
#endif
}

-- | Note that 'docTest' can be called only once per process execution
--
-- You only need to give paths to modules that are not imported from any other module

docTest::[FilePath] -- ^ Paths to root modules
         -> [String] -- ^ Options passed to ghci
         -> IO Test
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
