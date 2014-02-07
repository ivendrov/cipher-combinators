module Util where

import Data.Char

type Bit = Bool
xor1 :: Bit -> Bit -> Bit
xor1 True = not . id
xor1 False = id

xor :: [Bit] -> [Bit] -> [Bit]
xor = zipWith xor1


-- utility list functions

-- chop n splits a list into sublists of size n, 
-- with the last list potentially smaller
chop n [] = []
chop n l = let (first, rest) = splitAt n l
	   in first : chop n rest

	   
-- IO with bits 
showBit :: Bit -> Char
showBit = head . show. fromEnum

readBit :: Char -> Bit
readBit = toEnum . read . return

-- showBits n shows the given bit string
-- in chunks of n
showBits :: Int -> [Bit] -> String
showBits n = unwords . map (map showBit) . chop n

-- readBits reads a given bit string
-- ignoring whitespace
readBits :: String -> [Bit]
readBits = map (readBit) . filter (not . isSpace)