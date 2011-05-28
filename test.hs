
{-
    Copyright 2010, 2011 Pavel Lepin
    
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

import Prelude (($), (.), flip, uncurry, putStrLn, Integer, String)
import Control.Monad
import Data.List
import Data.TotalRecall
import Text.XML.HaXml.Haskell2Xml

k a b = run $ do
    let a' = "first parameter" |- a
    let b' = "second parameter" |- b
    let constValue = "some const value" |-- 1
    let c = "intermediate value" |- (a' + b')
    return
        (branch (a' == constValue)
            a'
            c
        )

commissionPercent :: Value String -> Value Integer -> Value Integer -> Value Integer -> Value Integer -> Value Integer
commissionPercent teamName experience grossProfit callsTaken averageMark = run $ do
    let teamName' = "Agent's Team" |- teamName
    let experience' = "Agent's Experience" |- experience
    let grossProfit' = "GP" |- grossProfit
    let callsTaken' = "Calls Taken" |- callsTaken
    let averageMark' = "Avg. Mark" |- averageMark
    let alpha = "Alpha Team" |-- "Alpha"
    let beta = "Beta Team" |-- "Beta"
    return
        (branch (teamName' == alpha)
            (commissionPercentAlpha experience' grossProfit' callsTaken' averageMark')
            (branch (teamName' == beta)
                (commissionPercentBeta experience' grossProfit' callsTaken' averageMark')
                (commissionPercentDefault experience' grossProfit' callsTaken' averageMark')
            )
        )

commissionPercentAlpha experience grossProfit callsTaken _ = run $ do
    let gpPerCall = "GP / Call" |- (grossProfit / callsTaken)
    return
        (branch (experience <= ("Alpha Grace Period In Months" |-- 3))
            ("Alpha Flat Scheme" |-- 30)
            (branch ((gpPerCall >= ("Alpha High Performance Threshold" |-- 35)) && (callsTaken >= ("Alpha Calls Planned" |-- 120)))
                ("Alpha High Performance Scheme" |-- 35)
                ("Alpha Default Scheme" |-- 20)
            )
        )

commissionPercentBeta _ grossProfit _ averageMark = run $ do
    return
        (branch (grossProfit <= ("Beta High Performance Threshold" |-- 4275))
            ("Beta Low Performance Scheme" |-- 18)
            (branch (averageMark >= ("Beta Positive Feedback Threshold" |-- 8))
                ("Beta Etiquette Incentive Scheme" |-- 32)
                ("Beta Default Scheme" |-- 22)
            )
        )

commissionPercentDefault _ _ _ _ = run $ do
    return ("Default Commission Percent" |-- 25)

k1 = k ("a" |-- 1) ("b" |-- 2)

k2 = k ("a" |-- 2) ("b" |-- 2)

k3 = reduceToInteger $ k ("a" |-- 1)

k4 = reduceToInteger $ (flip k) ("b" |-- 2)

k5 = reduceToInteger k

comm1 = reduceToInteger $ commissionPercent ("John's Team" |-- "Gamma")

comm2 = commissionPercent ("Jeff's Team" |-- "Beta") ("Jeff's Experience" |-- 5) ("Jeff's Gross Profit" |-- 4813) ("Jeff's Calls" |-- 146) ("Jeff's Avg. Mark" |-- 7)

comm3 = reduceToInteger $ commissionPercent ("Sarah's Team" |-- "Alpha") ("Sarah's Experience" |-- 13)
    ("Sarah's Gross Profit" |-- 7101) ("Sarah's Calls" |-- 198)

main = mapM_ (uncurry $ \i -> fWriteXml ('k':i:".xml")) (zip ['1'..] [k1, k2, k3, k4, k5, comm1, comm2, comm3])

