import Data.Vector

data EncryptionScheme msg ct = EncryptionScheme
				{ encrypt :: msg -> ct, 
				 decrypt :: ct -> msg }
				 
type Block = [Bit]

  



permutationCipher :: [Int] -> EncryptionScheme Block Block
permutationCipher ints = 
  let v = fromList ints
  in EncryptionScheme (
-- for a permutation cipher, the keys are index maps   
-- type PermutationCipher a = BlockCipher a [Int] 

ecb :: BlockCipher -> ([a] -> [a])
cbc, cfb, ofb :: BlockCipher a -> [a] -> ([a] -> [a])

