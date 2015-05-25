
input :: [Int]
input = [1,2,3]

test_reverse :: [Int] -> Bool
test_reverse xs = reverse (reverse xs) == xs

dontchangecontext = do line <- fmap reverse getLine
                       putStrLn $ " "


showlist :: Show a => [a] -> String
showlist [] = ""
showlist (x:xs) = show x ++ showlist xs

lengthtest x xs y ys = length ((x:xs) ++ ys) == length (x:xs) + (length ys)
