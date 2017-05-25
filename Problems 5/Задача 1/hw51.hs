decomposition x = temp 1 x
    where
        temp _ 0 = [[]]
        temp n x = [ head:tail | head <- [n..x], tail <- temp head (x - head) ] 