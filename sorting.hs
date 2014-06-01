import Data.List
--
-- QUICKSORT
--
quicksort [] = []
quicksort (h:tl) = (quicksort (fst partition')) ++ [h] ++ (quicksort (snd partition'))
	where 
		partition' = partition (<h) tl	
--
-- BUBBLESORT
--
bubblesorter (x:y:xs)
	| x > y = y : bubblesorter (x:xs)
	| otherwise = x : bubblesorter (y:xs)
bubblesorter x = x

bubblesort [] 0 = []
bubblesort xs idx
	| idx == (length xs) = xs
	| otherwise =  bubblesort (bubblesorter xs) (idx + 1)

--
-- MERGESORT
--
merge xs [] = xs
merge [] xs = xs
merge (x:y) (x':y')
	| x < x' = x : merge y (x':y') 
	| otherwise = x' : merge (x:y) y'

mergesort [] = []
mergesort xs
	| length xs == 1 = xs
	|	otherwise = merge (mergesort (fst half)) (mergesort (snd half))
	where half = splitAt (floor (fromIntegral (length xs) / 2)) xs
