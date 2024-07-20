module TestParser where

import Control.Monad.Except (runExcept, Except)
import Lexer
import Parser
import SugarSyntax
import Test.HUnit
import Types
import Util
import qualified Data.Map as M

parseStr :: String -> Result (M.Map Name String, Name, Term)
parseStr progText = do
    (names, nextName, tokens) <- scan progText
    ast <- parse tokens
    return (swap names, nextName, ast)

getAst :: Result (M.Map Name String, Name, Term) -> Result Term
getAst result = do
    (_, _, ast) <- result
    return ast

parseUnsafe :: String -> Term
parseUnsafe s = case (runExcept . getAst . parseStr) s of
    Right t -> t
    Left e -> error (e M.empty)

testParse :: String -> String -> Term -> Test
testParse testDesc progText expectedAst =
    TestCase $ assertEqual testDesc (parseUnsafe progText) expectedAst

tests :: Test
tests = TestList $
    [ testParse "Simple expression 1" "x" (Var NoInfo (Name 0))
    , testParse "Simple expression 2" "x y"
        (App NoInfo (Var NoInfo (Name 0)) (Var NoInfo (Name 1)))
    , testParse "Simple expression 3" "| x . x"
        (Lam NoInfo (Name 0) Nothing (Var NoInfo (Name 0)))
    , testParse "Simple expression 4" "| x . x y"
        (Lam NoInfo (Name 0) Nothing (App NoInfo (Var NoInfo (Name 0))
            (Var NoInfo (Name 1))))
    , testParse "Simple expression 5" "(| x . x) y"
        (App NoInfo (Lam NoInfo (Name 0) Nothing (Var NoInfo (Name 0)))
            (Var NoInfo (Name 1)))
    , testParse "Simple expression 6" "nil nil nil nil"
        (App NoInfo (App NoInfo (App NoInfo (Nil NoInfo) (Nil NoInfo))
            (Nil NoInfo)) (Nil NoInfo))
    , testParse "Simple expression 7" "(x : @ -> @)"
        (Tag NoInfo (TFunc TTree TTree) (Var NoInfo (Name 0)))
    , testParse "Simple expression 8" "< x y"
        (App NoInfo (App NoInfo (Hd NoInfo) (Var NoInfo (Name 0)))
            (Var NoInfo (Name 1)))
    ]
