module Lib
  ( someFunc,
  )
where

import BinaryTree
import Data.Function

someFunc :: IO ()
someFunc = do
  print $ Empty & insert (5 :: Integer) & insert 3 & insert 7 & insert 1 & insert 4 & insert 6
