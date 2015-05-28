x = 23

showlist1 :: [t] -> String
showlist1 x = "not implemented"

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

data MonoidInst = Elem1 | Elem2

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty


instance Monoid MonoidInst where
  mappend m1 m2 = Elem1
  mempty = Elem1
