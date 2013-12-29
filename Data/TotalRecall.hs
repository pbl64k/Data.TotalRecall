{-# OPTIONS -fglasgow-exts #-}

{-
    Copyright 2010, 2011, 2013 Pavel Lepin
    
    This file is part of Data.TotalRecall.
    
    Data.TotalRecall is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.
    
    Data.TotalRecall is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.
    
    You should have received a copy of the GNU General Public License
    along with Data.TotalRecall.  If not, see <http://www.gnu.org/licenses/>.
-}

module Data.TotalRecall (
    Value,
    Description(dumpDescription),
    Reducible(reduce),
    reduceToInteger,
    (|-),
    (|--),
    lift1,
    lift2,
    lift3,
    (~!),
    (&&),
    (||),
    (~-),
    (+),
    (-),
    (*),
    (/),
    (//),
    (\\),
    (==),
    (<),
    (>),
    (>=),
    (<=),
    (>=<),
    branch,
    run,
    ($),
    (.)
    ) where

import Prelude (id, ($), (.), flip, (++))
import qualified Prelude as P
import qualified Control.Monad as M
import qualified Data.List as L
import qualified Data.Maybe as Mb

import Text.XML.HaXml.Haskell2Xml

class Haskell2Xml a => Description a where
    dumpDescription :: a -> P.String

data BoxedDescription = forall a. Description a => BoxedDescription a

instance Description BoxedDescription where
    dumpDescription (BoxedDescription description) = dumpDescription description

instance Haskell2Xml BoxedDescription where
    toHType _ = String -- we don' need no stinkin' DTDs.
    toContents (BoxedDescription description) = toContents description
    fromContents _ = P.undefined

data StructuredDescription = StructuredDescription P.String [BoxedDescription]

instance Description StructuredDescription where
    dumpDescription (StructuredDescription descStr nestedDescList) = L.foldl
        (\acc (ix, nestedDesc) -> acc ++ " " ++ P.show ix ++ ":(" ++ dumpDescription nestedDesc ++ ")")
        ("{" ++ descStr ++ "}") (L.zip [1..] nestedDescList)

instance Haskell2Xml StructuredDescription where
    toHType _ = String
    toContents (StructuredDescription descStr nestedDescList) = [CElem (Elem "structured-description" []
        [CElem (Elem "textual-description" [] [CString P.True descStr]),
        CElem (Elem "inputs" [] (nestedDescList M.>>= toContents))]
        )]
    fromContents _ = P.undefined

data P.Show a => Value a = forall b. Description b => Value (Mb.Maybe a) b

dumpMaybeValue :: P.Show a => Mb.Maybe a -> P.String
dumpMaybeValue (Mb.Just value) = P.show value
dumpMaybeValue Mb.Nothing = "???"

instance P.Show a => Description (Value a) where
    dumpDescription (Value value description) = dumpMaybeValue value ++
        " [" ++ dumpDescription description ++ "]"

instance P.Show a => Haskell2Xml (Value a) where
    toHType _ = String
    toContents (Value value description) = [CElem (Elem "value" [] [
        CElem (Elem "calculated-value" [] [CString P.True $ dumpMaybeValue value]),
        CElem (Elem "calculation-description" [] (toContents description))
        ])]
    fromContents _ = P.undefined

(|-) :: P.Show a => P.String -> Value a -> Value a
(|-) description desc'@(Value value _) = Value value
    (StructuredDescription description [BoxedDescription desc'])

(|--) :: P.Show a => P.String -> a -> Value a
(|--) description value = Value (M.return value) (StructuredDescription description [])

lift1 :: (P.Show a, P.Show b) => P.String -> (a -> b) -> (Value a -> Value b)
lift1 description f descX@(Value x _) =
    Value ((M.liftM f) x) (StructuredDescription description [BoxedDescription descX])

lift2 :: (P.Show a, P.Show b, P.Show c) => P.String -> (a -> b -> c) ->
    (Value a -> Value b -> Value c)
lift2 description f descX@(Value x _) descY@(Value y _) = Value ((M.liftM2 f) x y)
    (StructuredDescription description [BoxedDescription descX, BoxedDescription descY])

lift3 :: (P.Show a, P.Show b, P.Show c, P.Show d) => P.String ->
    (a -> b -> c -> d) -> (Value a -> Value b -> Value c -> Value d)
lift3 description f descX@(Value x _) descY@(Value y _) descZ@(Value z _) =
    Value ((M.liftM3 f) x y z) (StructuredDescription description
        [BoxedDescription descX, BoxedDescription descY, BoxedDescription descZ])

(~!) = lift1 "not" P.not
(&&) = lift2 "all of" (P.&&)
(||) = lift2 "any of" (P.||)

(~-) = lift1 "-" P.negate
(+) = lift2 "+" (P.+)
(-) = lift2 "-" (P.-)
(*) = lift2 "*" (P.*)
(//) = lift2 "/" (P./)
(/) = lift2 "/" P.div
(\\) = lift2 "modulo" P.mod

(==) :: (P.Show a, P.Eq a) => Value a -> Value a -> Value P.Bool
(==) = lift2 "equals" (P.==)
(>) :: (P.Show a, P.Ord a) => Value a -> Value a -> Value P.Bool
(>) = lift2 "greater than" (P.>)
(<) :: (P.Show a, P.Ord a) => Value a -> Value a -> Value P.Bool
(<) = lift2 "less than" (P.<)
(>=) :: (P.Show a, P.Ord a) => Value a -> Value a -> Value P.Bool
(>=) = lift2 "greater than or equals" (P.>=)
(<=) :: (P.Show a, P.Ord a) => Value a -> Value a -> Value P.Bool
(<=) = lift2 "less than or equals" (P.<=)
(>=<) :: (P.Show a, P.Ord a) => Value a -> Value a -> Value P.Ordering
(>=<) = lift2 "compared with" P.compare

descBranch :: P.Show a => Value P.Bool -> Value a -> Value a -> StructuredDescription
descBranch descP descA descB = StructuredDescription "if .. then .. else"
    [BoxedDescription descP, BoxedDescription descA, BoxedDescription descB]

branch :: P.Show a => Value P.Bool -> Value a -> Value a -> Value a
branch descP@(Value (Mb.Just P.True) _) descA@(Value a _) descB = Value a (descBranch descP descA descB)
branch descP@(Value (Mb.Just P.False) _) descA descB@(Value b _) = Value b (descBranch descP descA descB)
branch descP@(Value Mb.Nothing _) descA descB = Value Mb.Nothing (descBranch descP descA descB)

data Identity a = Identity a

run :: Identity a -> a
run (Identity x) = x

instance M.Monad Identity where
    return = Identity
    (>>=) = (flip ($)) . run

class Reducible a b where
    reduce :: a -> b

instance P.Show a => Reducible (Value a) (Value a) where
    reduce = id

instance (P.Show a, Reducible b c) => Reducible (Value a -> b) c where
    reduce x = reduce (x (Value P.Nothing (StructuredDescription "(unspecified value)" [])))

reduceToInteger :: Reducible a (Value P.Integer) => a -> Value P.Integer
reduceToInteger = reduce

