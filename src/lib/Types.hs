module Types where

import Util
import qualified Data.Set as S
import Control.Monad (foldM)
import Control.Monad.State

data Type = TTree | TFunc Type Type | TVar Name deriving (Eq, Ord)

instance Show Type where
    show t = case t of
        TTree               -> "@"
        TVar   x            -> show x
        TFunc  TTree   tR   -> "@ -> " ++ show tR
        TFunc (TVar x) tR   -> show x ++ " -> " ++ show tR
        TFunc tL    TTree   -> "(" ++ show tL ++ ") -> @"
        TFunc tL   (TVar x) -> "(" ++ show tL ++ ") -> " ++ show x
        TFunc tL    tR      -> "(" ++ show tL ++ ") -> (" ++ show tR ++ ")"

instance PrettyPrint Type where
    prettyPrint names ty = case ty of
        TTree     -> "@"
        TVar  x   -> names !? x
        TFunc a r ->
            "(" ++ prettyPrint names a ++ " -> " ++ prettyPrint names r ++ ")"
