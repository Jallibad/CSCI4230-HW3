{-# LANGUAGE ViewPatterns #-}

import Data.Bits (testBit, xor)
import MathFunctions
import Text.Printf (printf)

p = 499
q = 547
a = -57
b = 52
n = p*q

encrypt :: String -> Integer -> (String, Integer)
encrypt (toBoolList -> m) x0 = (fromBoolList c, keyStream !! (length m))
	where
		keyStream = bbs x0
		c = zipWith xor m $ map (`testBit` 0) keyStream

decrypt :: (String, Integer) -> String
decrypt (toBoolList -> c, y) = fromBoolList $ zipWith xor c x
	where
		t = length c - 1
		d n = modularPow ((n+1) `div` 4) (t+1) (n-1)
		u = modularPow y (d p) p
		v = modularPow y (d q) q
		x = map (`testBit` 0) $ bbs $ mod (v*a*p + u*b*q) n

bbs :: Integer -> [Integer]
bbs = iterate ((`mod` n) . (^2))

main = do
	let m = "10011100000100001100"
	let x0 = 159201 :: Integer
	printf "1. C(%s) = %s\n" m (fst $ encrypt m x0)
	printf "2. D(C(%s)) = %s\n" m (decrypt $ encrypt m x0)