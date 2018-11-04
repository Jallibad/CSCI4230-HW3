import Data.Bits (testBit, xor)

toBoolList :: String -> [Bool]
toBoolList = map (=='1')

fromBoolList :: [Bool] -> String
fromBoolList = map $ \x -> if x then '1' else '0'

p = 499
q = 547
n = p*q
m = "10011100000100001100"
x0 = 159201

modularPow :: (Integral a, Integral b) => a -> b -> a -> a
modularPow _ 0 _ = 1
modularPow b e m = if odd e then mult result b else result
	where
		result = modularPow (mult b b) (e `div` 2) m
		mult x y = (x*y) `mod` m

blumGoldwasserEncryption :: [Bool] -> Integer -> ([Bool], Integer)
blumGoldwasserEncryption m x0 = (c, keyStream !! (length m - 1))
	where
		keyStream = bbs x0
		c = zipWith xor m $ map (`testBit` 0) keyStream

blumGoldwasserDecryption :: ([Bool], Integer) -> [Bool]
blumGoldwasserDecryption (c, y) = zipWith xor c b
	where
		l = length c - 1
		b = map (`testBit` 0) $ bbs x0
		rp = modularPow y (((p+1) `div` 4)^l) p
		rq = modularPow y (((q+1) `div` 4)^l) q
		x0 = (q*(modInverse q p)*rp + p*(modInverse p q)*rq) `mod` n

bbs :: Integer -> [Integer]
bbs = iterate ((`mod` n) . (^2))

diffs _ [] = []
diffs _ (_:[]) = []
diffs f (x:y:xs) = (f x y) : diffs f (y:xs)

modInverse :: Integral a => a -> a -> a
modInverse a b = (last $ init s) `mod` b
	where
		r = takeWhile (/=0) $ a : b : zipWith (-) r (zipWith (*) q $ tail r)
		s = 1 : 0 : zipWith (-) s (zipWith (*) q (tail s))
		q = diffs div r