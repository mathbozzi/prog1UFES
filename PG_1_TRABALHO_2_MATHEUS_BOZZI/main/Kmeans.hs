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
kMeansCoordHelp::[([Double],String)] -> [String] -> [[Double]]
kMeansCoordHelp list names = [ kMeansClass name list | name <- names ]

-- funcao que recebe uma lista de lista de dados de treino (coord e classe)
-- e calcula o centro de massa para cada fold, essa funcao utiliza
-- a kMeansCoordHelp que calcula o centro de massa para cada fold separado
-- e nessa funcao ele recebe cada um e coloca em uma lista de lista de lista de doubles(Centro Massa)
kMeansCoord::[[([Double],String)]] -> [String] -> [[[Double]]]
kMeansCoord list names = [ kMeansCoordHelp l names | l <- list ]

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

-- funcao que recebe uma lista de lista de double e uma lista de string
-- e junta(zip) cada lista de double com a string formando assim uma tupla
-- onde o primeiro elemento eh a lista de double(centro de massa) e o segundo o nome da classe
joinCoordKmeansFoldHelp:: [[Double]] -> [String] -> [([Double],String)]
joinCoordKmeansFoldHelp list classifiers = zip list classifiers

-- funcao que recebe uma lista de lista de lista de double e uma lista de string
-- e itera sobre cada fold, juntando a lista(centro de massa) com a string (classificacao)
-- para o algoritimo do centroide usar o Vizinho mais proximo e fazer a classificacao
joinCoordKmeansFold:: [[[Double]]] -> [String] -> [[([Double],String)]]
joinCoordKmeansFold list classifiers = [joinCoordKmeansFoldHelp l classifiers | l <- list]