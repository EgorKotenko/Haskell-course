fibRow = take 20 fib where
   fib0 = 0 : fib
   fib1 = zipWith (+) fib0 fib
   fib = 1 : fib1