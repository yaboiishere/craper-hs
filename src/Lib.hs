module Lib
  ( someFunc,
  )
where

import Parser

someFunc :: IO ()
someFunc = do
  -- print $ Empty & insert (5 :: Integer) & insert 3 & insert 7 & insert 2 & insert 4 & insert 6 & insert 8
  doTest'
