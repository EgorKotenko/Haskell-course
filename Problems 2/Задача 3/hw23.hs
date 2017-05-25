digitSum :: Int -> Int
digitSum 0 = 0;
digitSum n = rem n 10 + digitSum (quot n 10)