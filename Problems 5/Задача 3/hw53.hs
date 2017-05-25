import Control.Monad

combinations length = forM [1..length] $ \x -> [1,2,3] >>= flip (:) []

