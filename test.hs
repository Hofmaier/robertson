
input :: [Int]
input = [1,2,3]

test_reverse :: [Int] -> Bool
test_reverse xs = reverse (reverse xs) == xs

dontchangecontext = do line <- fmap reverse getLine
                       putStrLn $ " "
