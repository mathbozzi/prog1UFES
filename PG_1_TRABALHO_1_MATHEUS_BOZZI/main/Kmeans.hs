module Kmeans where
import Helps
import Data.List

-- funcao que verifica entre todos os elementos da lista e remove os duplicados
-- ela em a funcao de retornar uma lista de Strings, com os nomes de todas as classes
-- existentes no arquivo de entrada
-- nub é responsavel por remover elementos repetidos
verifyClass::[([Double],String)] -> [String]
verifyClass xs = nub (map snd xs)

-- funcao que recebe um conjunto de treino e retorna uma lista de lista de coordenadas
-- no qual essas coordenadas são o centro de massa de acordo com a quantidade de classes
-- do conjunto de treinamento
kMeansCoord::[([Double],String)] -> [String] -> [[Double]]
kMeansCoord list names = [ kMeansClass name list | name <- names ]

-- funcao que recebe uma string e um conjunto de treino e retorna uma lista de 
-- coordenadas, que sao o centro de massa
kMeansClass::String -> [([Double], String)] -> [Double]
kMeansClass name list =
    joinCoords [coordList ? (read (show(takeSize list name)) :: Double) | (coordList, n) <- list, n == name]

-- funcao que recebe uma lista de listas de coordenadas e recursivamente
-- junta todos os elementos para somar as listas para o calculo do centro de massa
joinCoords::(Num a) => [[a]] -> [a]
joinCoords [] = []
joinCoords (x:xs) = (joinCoords xs) `joinCoordsAux` x

-- funcao que recebe um conjunto de treino e verifica o tamanho da lista de acordo 
-- com o nome da classe, a funcao retorna um valor inteiro com o tamanho
takeSize :: [([Double],String)] -> String -> Int
takeSize list name = sum [1 | (_, name2) <- list, name2 == name]                     

-- funcao que recebe uma lista de cordenadas e soma o valor de cada indice da lista de
-- coordenadas,  e retorna a soma de cada coordenada ja somada pelo indice
joinCoordsAux::(Num a) => [a] -> [a] -> [a]
joinCoordsAux list [] = list
joinCoordsAux [] list2 = list2
joinCoordsAux list list2 = [ (list !! i) + (list2 !! i) | i <- [0..(size list) - 1] ]

-- funcao que recebe a lista de coordenada divide cada elemento da lista pelo numero do tamanho de Rn
(?)::(Fractional a) => [a] -> a -> [a]
list ? numerator = [(denominator)/(numerator) | denominator<-list ]

