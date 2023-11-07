module Parser where

data PSymbol = String | IntConst | Text | Semicolon | Period | Quotes | Othersy deriving (Show, Eq)

type PDataFile = [PRecord]

type PRecord = [PField]

data PField = PInteger [Int] | PString [PText] deriving (Show, Eq)

type PText = [PChar]

data PChar = Letter Char | Digit Char deriving (Show, Eq)

-- 1234;631;"Hello".5678;"cource1"."end".
-- The example input should return [IntConst, Semicolon, IntConst, Semicolon, Text, Period, IntConst, Semicolon, Text, Period]

test :: String
test = "1234;631;\"Hello\".5678;\"cource1\".\"end\"."

doTest :: IO ()
doTest = print $ parse test

parse :: String -> [PSymbol]
parse "" = []
parse input = symbol : parse rest
  where
    (symbol, rest) = parseSymbol input

parseSymbol :: String -> (PSymbol, String)
parseSymbol [] = (Othersy, [])
parseSymbol (x : xs)
  | x == ';' = (Semicolon, xs)
  | x == '.' = (Period, xs)
  | x == '"' = parseText xs
  | isDigit x = parseInteger (x : xs)
  | otherwise = (Othersy, xs)

parseText :: String -> (PSymbol, String)
parseText [] = (Othersy, [])
parseText (x : xs)
  | x == '"' = (Text, xs)
  | otherwise = parseText xs

parseInteger :: String -> (PSymbol, String)
parseInteger [] = (Othersy, [])
parseInteger (x : xs)
  | isDigit x = parseInteger xs
  | otherwise = (IntConst, x : xs)

isDigit :: Char -> Bool
isDigit x = x `elem` ['0' .. '9']

isLetter :: Char -> Bool
isLetter x = x `elem` ['a' .. 'z'] ++ ['A' .. 'Z']

-- 1234;631;"Hello".5678;"cource1"."end".
-- The example input should return [IntConst 1234, Semicolon, IntConst 631, Semicolon, Text "Hello", Period, IntConst 5678, Semicolon, Text "cource1", Period, Text "end", Period]

-- This version returns almost the same result as the previous one, but it retains the values of the integer and text constants.

data PSymbol' = String' String | IntConst' String | Text' String | Semicolon' | Period' | Quotes' | Othersy' deriving (Show, Eq)

doTest' :: IO ()
doTest' = print $ parse' test

parse' :: String -> [PSymbol']
parse' "" = []
parse' input = symbol : parse' rest
  where
    (symbol, rest) = parseSymbol' input

parseSymbol' :: String -> (PSymbol', String)
parseSymbol' [] = (Othersy', [])
parseSymbol' (x : xs)
  | x == ';' = (Semicolon', xs)
  | x == '.' = (Period', xs)
  | x == '"' = parseText' xs
  | isDigit x = parseInteger' (x : xs)
  | otherwise = (Othersy', xs)

parseText' :: String -> (PSymbol', String)
parseText' xs = parseText'' xs []

parseText'' :: String -> String -> (PSymbol', String)
parseText'' [] _acc = (Othersy', [])
parseText'' (x : xs) acc
  | x == '"' = (Text' acc, xs)
  | otherwise = parseText'' xs (acc ++ [x])

parseInteger' :: String -> (PSymbol', String)
parseInteger' xs = parseInteger'' xs []

parseInteger'' :: String -> String -> (PSymbol', String)
parseInteger'' [] _acc = (Othersy', [])
parseInteger'' (x : xs) acc
  | isDigit x = parseInteger'' xs (acc ++ [x])
  | otherwise = (IntConst' acc, x : xs)
