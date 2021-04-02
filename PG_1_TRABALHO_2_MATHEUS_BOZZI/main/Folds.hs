module Folds where
import Helps

-- funcao que recebe uma lista de lista de int, o numero de fold
-- e o resto da divisao, para assim fazer o conjunto de folds pelos
-- numeros randomicos e retornando os Folds com seus indices
unionFolds :: [[Int]] -> Int -> Int -> [[Int]]
unionFolds list nFolds remnant = if remnant == 0 then list
                    else if (size list)-nFolds >= 2 then unionFolds (initFunc(initFunc list) ++ [(last (initFunc list)) ++ (last list)]) nFolds remnant 
                    else initFunc((insertFold remnant list) ++ reverse (take ((size list)-remnant) (reverse list)))

-- funcao que recebe o resto e uma lista de lista de indices
-- e retorna uma lista de lista de indices, porem adiciona os
-- indices restantes ao inicio de cada lista de fold
insertFold :: Int -> [[Int]] -> [[Int]]
insertFold remnant list = joinList (zip (take remnant list) (last list))

--funcao que recebe uma lista de tupla zipada e percorre
-- a lista para retornar a lista de folds com os indices corretos 
joinList :: [([Int],Int)] -> [[Int]]
joinList = map mountList

-- funcao que recebe uma tupla com uma lista de indices
-- e um indice, e entao adiciona ao final da lista 
-- retornando assim o a lista com o indice ao final
mountList :: ([Int], Int) -> [Int]
mountList (x,y) = x ++ [y]

-- funcao que recebe um numero inteiro e uma lista de numeros randomSet(int)
-- e separa os elementos dividindo pelo numero inteiro
breakFolds :: Int -> [Int] -> [[Int]]
breakFolds nInt list
               | nInt <= 0 || null list = []
               | otherwise = (take nInt list):(breakFolds nInt (drop nInt list))

-- funcao que recebe uma lista de folds e calcula o tamanho
-- de cada lista dos fold e retorna uma lista de int com o tamanho 
sizeFold :: [[Int]] -> [Int]
sizeFold list = [sizeofFold x | x <- list]

-- funcao que calcula o tamanho de cada fold separado
sizeofFold :: [Int] -> Int
sizeofFold fold = size fold