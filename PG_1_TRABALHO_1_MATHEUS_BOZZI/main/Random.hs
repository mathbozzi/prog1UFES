module Random where
import System.IO
import System.Random

tem :: Eq a => a -> [a] -> Bool
tem _ [] = False
tem y (x:xs)
   | x == y = True
   | otherwise = tem y xs

removeDup :: Eq a => [a] -> [a]
removeDup l = removeD l []
   where
     removeD [] _ = []
     removeD (x:xs) ls
        | tem x ls = removeD xs ls
        | otherwise = x: removeD xs (x:ls)

randomSet :: Int -> Int -> Int -> [Int]
randomSet ntest size seed =
   take ntest (removeDup ((randomRs (0,size-1) (mkStdGen seed) :: [Int])))

-- funcao dada pelo professor, só fiz algumas modificações pra receber os parametros de nTeste, tamanho e semente
