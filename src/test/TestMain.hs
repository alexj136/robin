module Main where

import Test.HUnit
import qualified System.Exit as Exit
--import qualified TestParser as P
import qualified TestSugarSyntax as SS
import qualified TestTypeCheck as TC

tests :: Test
tests = TestList [{-P.tests,-} SS.tests, TC.tests]

main :: IO ()
main = do
    result <- runTestTT tests
    if failures result > 0 then Exit.exitFailure else Exit.exitSuccess
