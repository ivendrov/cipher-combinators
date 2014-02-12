import Data.List
import Data.Ord (comparing)

import Util






-- | BLOCK CIPHERS

type Block = [Bit]
type BlockCipher = Cipher Block Block

  

-- | PERMUTATION CIPHERS

-- permutationCipher sigma returns a permutation cipher where
-- the (sigma !! i)th letter becomes the ith
permutationCipher :: [Int] -> Cipher [a] [a]
permutationCipher ints = 
  let permute sigma = map snd . sortBy (comparing fst) . zip sigma
      backpermute sigma a = map (a!!) sigma
  in Cipher 
         (backpermute ints)
         (permute ints)
  


-- | MODES OF OPERATION

ecb :: Cipher a a -> Cipher [a] [a]
ecb (Cipher e d) = Cipher (map e) (map d)

cbc, ofb :: Cipher Block Block -> Block -> Cipher [Block] [Block]
-- Cipherblock Chaining Mode
cbc (Cipher e d) iv = 
    let encrypt msg = ciphertext
            where ciphertext = map e . zipWith xor (iv : ciphertext) $ msg
        decrypt ciphertext = msg
            where msg = zipWith xor (iv : ciphertext) . map d $ ciphertext
    in
      Cipher encrypt decrypt

-- Output Feedback Mode (note that the message block size r can be smaller
-- than the initial value block size n; only the first r bits will be used)
ofb (Cipher e d) iv = 
    Cipher (zipWith xor stream) (zipWith xor stream)
        where stream = tail (iterate e iv)    


-- Cipher Feedback Mode (has additional parameter r = msg block size)
cfb :: Cipher Block Block -> Int -> Block -> Cipher [Block] [Block]
cfb (Cipher e d) r iv = 
    let encrypt msg = ciphertext 
            where ciphertext = zipWith xor msg (map e is)
                  is = iv : zipWith ((++) . drop r) is ciphertext -- internal state
        decrypt ciphertext = msg
            where msg = zipWith xor ciphertext (map e is)
                  is = iv : zipWith ((++) . drop r) is ciphertext
    in 
      Cipher encrypt decrypt
    


-- example
plaintext = readBits "1001 1011 0010"
cipher = permutationCipher [1,2,3,0]
iv = readBits "1111"
ciphertext = concat . decrypt (cfb cipher 3 iv) . chop 3 $ plaintext
newPlain = concat . encrypt (cfb cipher 3 iv) . chop 3 $ ciphertext




