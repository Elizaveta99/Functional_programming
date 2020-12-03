transpositions :: (Eq a) => [a] -> [[a]]
transpositions [] = [[]]
transpositions l = [a:x | a <- l, x <- (transpositions $ filter (\x -> x /= a) l)]