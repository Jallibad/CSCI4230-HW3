module MathFunctions where

modularPow :: (Integral a, Integral b) => a -> b -> a -> a
modularPow _ 0 _ = 1
modularPow b e m = if odd e then mult result b else result
	where
		result = modularPow (mult b b) (e `div` 2) m
		mult x y = (x*y) `mod` m

diffs :: (a -> a -> b) -> [a] -> [b]
diffs _ [] = []
diffs _ (_:[]) = []
diffs f (x:y:xs) = (f x y) : diffs f (y:xs)

modInverse :: Integral a => a -> a -> a
modInverse a b = (last $ init s) `mod` b
	where
		r = takeWhile (/=0) $ a : b : zipWith (-) r (zipWith (*) q $ tail r)
		s = 1 : 0 : (zipWith (-) s $ zipWith (*) q $ tail s)
		q = diffs div r

toBoolList :: String -> [Bool]
toBoolList = map (=='1')

fromBoolList :: [Bool] -> String
fromBoolList = map $ \x -> if x then '1' else '0'