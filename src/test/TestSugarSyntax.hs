module TestSugarSyntax where

import SugarSyntax
import Test.HUnit
import qualified DeBruijnSyntax as P

tests :: Test
tests = TestCase test_helloWorld

test_helloWorld :: Assertion
test_helloWorld = do
    assertEqual "helloWorld" True False
