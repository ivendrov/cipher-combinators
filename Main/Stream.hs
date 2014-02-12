import Util
import Data.List

linearRecursion :: [Bit] -> [Bit] -> [Bit]
-- linearRecursion c k generates an infinite
-- stream of bits starting with k and using 
-- the coefficients c
linearRecursion c k = s where
    s = k ++ zipWith combine (tails s) (repeat (reverse c))
    combine l1  = sumMod2 . zipWith (&&) l1
    sumMod2 = foldl1 (xor1)
    


streamCipher :: [Bit] -> Cipher [Bit] [Bit]
streamCipher stream = Cipher e e where
  e = xor stream
  
  
k = readBits "1010101"
c = readBits "1110011"

  
stream = linearRecursion c k
cipher = streamCipher stream
plaintext = readBits "1110001 1110001 1110001"
ciphertext = encrypt cipher plaintext
newPlain = decrypt cipher ciphertext

