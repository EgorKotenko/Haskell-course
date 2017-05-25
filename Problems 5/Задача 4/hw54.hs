import Control.Monad

products length = [1..length] >>= \x -> [1..length] >>= \y -> return (x*y)