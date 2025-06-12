lluviasEnero :: [Int]
lluviasEnero = [0,2,5,1,34,2,0,21,0,0,0,5,9,18,4,0]

--

diasLluvia1 :: [Int] -> [Int]
diasLluvia1 = dropWhile (== 0)

diasLluvia2 :: [Int] -> [Int]
diasLluvia2 = takeWhile (/= 0)

--MUY DIFICIL, NO LO HICIMOS