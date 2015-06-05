input = [1,2,3]
test_reverse :: Bool
test_reverse = reverse (reverse input) == input

length1 [x] = 1

x = 1
p1 = length [x] == 1

length' [] = 0
length' (x:xs) = 1 + length' xs

concat' [] xs = xs
concat' (x:xs) ys = x:(xs++ys)
