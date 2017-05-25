buildBinList :: Int -> [Int]
buildBinList 1 = [2]
buildBinList n = 2 ^ n : buildBinList (n - 1)

