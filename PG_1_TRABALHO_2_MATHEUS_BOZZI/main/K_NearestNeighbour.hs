module K_NearestNeighbour where
import NearestNeighbour
import Data.List
import Data.Function

-- funcao que recebe uma lista de folds de treino, uma lista de folds de teste um K para o calculo
-- do algoritimo de K-vizinhos, essa funcao itera  sobre cada fold e atribui uma classificacao 
-- pelos K vizinhos mais proximos do dado lido e retorna uma lista de lista de dados classificados para cada fold 
k_NearNeighbour :: [[([Double],String)]] -> [[([Double],String)]] -> Int -> [[(([Double],String),String)]]
k_NearNeighbour (x:[]) (y:[]) k = [k_NearNeighbourHelp (takeKNeighOrderByLength x y k)]
k_NearNeighbour (x:xs) (y:ys) k = [k_NearNeighbourHelp (takeKNeighOrderByLength x y k)] ++ k_NearNeighbour xs ys k

-- essa funcao recebe uma lista de tuplas com cada tupla sendo tendo na primeira parte o dado 
-- e a segunda parte eh a lista de K vizinhos mais proximos dele, para assim atribuir a classe
-- e retornando assum uma lista de tupls com o primeiro elemento sendo o dado e o segundo 
-- elemento a sua classificacao, para cada fold separado
k_NearNeighbourHelp :: [(([Double],String),[[([Double],String)]])] -> [(([Double],String),String)]
k_NearNeighbourHelp xs = [ attribClassFold (fst x) (snd x) | x <- xs]

-- funcao que recebe um dado e uma lista com K vizinhos mais proximos 
-- e utiliza o algoritimo do vizinho mais proximo e atribui a classificacao
-- retornando assim uma tupla com o (elemento, e sua classificacao) 
attribClassFold :: ([Double],String) -> [[([Double],String)]] -> (([Double],String),String)
attribClassFold test (x:xs) = if (length x) >= 2
                              then attribClass test x (computeDist (fst test) (fst (head x))) (snd (head x))
                              else (test, (snd (head x)))

-- funcao que recebe a lista de treino, de teste e o K, entao calcula os K vizinhos mais proximos
-- de cada Conjunto e retorna uma lista de tuplas, onde o primeiro elemento eh o conjunto testado
-- e o snd eh a lista dos k vizinhos mais proximos, ordenados pelo tamanho de maior para menor
takeKNeighOrderByLength ::[([Double],String)] -> [([Double],String)] ->  Int  -> [(([Double],String),[[([Double],String)]])] 
takeKNeighOrderByLength trainingList testList k = [ (test, (reverse (sortBy (compare `on` length) (groupAndSortByName (takeNearestsK test trainingList k)))) ) | test <- testList ]


-- funcao que recebe um teste ([1,2,3],"Setosa"), e uma lista de treino e o K, e entao 
-- calcula os K vizinhos mais proximos da coordenada de teste passada, retornando assim
-- os k elementos mais proximos em uma lista
takeNearestsK :: ([Double],String) -> [([Double],String)] -> Int -> [([Double],String)]
takeNearestsK _ _ 0  = []
takeNearestsK _ [] _ = []
takeNearestsK test trainingList k = (addNearest test trainingList):takeNearestsK test (trainingList \\ [addNearest test trainingList]) (k-1)

-- funcao que recebe um teste ([1,2,3],"Setosa"), e uma lista de treino, e entao
-- calcula o vizinho mais proximo e retorna ele
addNearest :: ([Double],String) -> [([Double],String)] -> ([Double],String)
addNearest test trainingList = attribClassHelp test trainingList (euclidAlgorth (fst test) (fst (head trainingList))) (head trainingList)


-- essa funcao recebe uma lista de Coordenadas e o nome, e ordena pelo nome
-- apos isso, agupa os que tem o mesmo nome em listas separadas
-- retornando assim uma lista de listas de conjuntos separados pelo nome
groupAndSortByName :: [([Double],String)] -> [[([Double],String)]]
groupAndSortByName xs = groupBy (\x y -> (snd x) == (snd y)) (sortBy (compare `on` \(a,b)->b) xs)

