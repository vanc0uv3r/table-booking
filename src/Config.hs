module Config
    (
        openH,
        closeH,
        interval,
        tableNum,
        bookInterval,
        cnfName,
    ) where

import Data.Fixed

h1 :: Int
m1 :: Int
s1 :: Pico

h2 :: Int
m2 :: Int
s2 :: Pico

h1 = 10
m1 = 0
s1 = 0

h2 = 22
m2 = 0
s2 = 0

openH = (h1, m1, s1)
closeH = (h2, m2, s2)

interval = 3600
tableNum = 1
bookInterval = 7200

cnfName = "tables.txt"
