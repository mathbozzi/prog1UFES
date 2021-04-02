module Sets where
import Data.List

-- funcao que recebe uma lista de inteiros e uma lista de tuplas com coordenadas e nome da classe
-- e separa os elementos de acordo com o valor da lista de inteiros, montando assim duas
-- partes em uma tupla, uma é o conjunto de treino e a outra e o conjunto de teste
-- ([conjunto de treino],[conjunto de teste])
difierSet :: [[Int]] -> [([Double],String)] -> ([[([Double],String)]],[[([Double],String)]])
difierSet randomList objects = ([objects\\(takeIElement objects [] x) | x <- randomList],[(takeIElement objects [] x) | x <- randomList]) 

-- funcao que dado uma lista retorna o primeiro elemento
first :: [a] -> a
first (x:xs) = x

-- funcao que dado uma lista retorna a lista sem o primeiro elemento
remnant:: [a] -> [a]
remnant [] = []
remnant (x:xs) = xs

-- funcao que recebe um conjunto e separa ele pelo indice, ate que a lista esteja vazia,
-- separando assim os conjuntos de treino e teste
takeIElement :: [([Double],String)] -> [([Double],String)] -> [Int] -> [([Double],String)]
takeIElement _ element [] = element
takeIElement objects element list = takeIElement objects ((objects!!(lastF list)):element) (initF list) 

-- funcao que pega o ultimo elemento da lista
lastF :: [a] -> a
lastF [x] = x
lastF (_:xs) = lastF xs

-- pega todos os elementos sem ultimo dalista
initF:: [a] -> [a]
initF [] = []
initF [x] = []
initF (x:xs) = x: initF xs