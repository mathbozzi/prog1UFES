module NearestNeighbour where

-- funcao que recebe um conjunto do algoritimo de vizinho mais proximo,
-- a sua classificacao resultante e um inteiro que é a soma,
-- retornando assim um inteiro com a quantidade de acertos 
accuracyNeighbour:: [(([Double],String),String)] -> Int -> Int
accuracyNeighbour [] cont = cont
accuracyNeighbour nNeighbour cont = if equals (snd (fst (head nNeighbour))) (snd (head nNeighbour))
                                    then accuracyNeighbour (tail nNeighbour) cont+1
                                    else accuracyNeighbour (tail nNeighbour) cont

-- funcao que verifica se duas strings são iguais e 
-- retorna um Booleano True para caso afirmativo e False para caso negativo
equals:: String -> String -> Bool
equals name1 name2 = if name1 == name2
                     then True
                     else False

-- funcao que recebe dois conjuntos, o de treino e o de teste, verifica a distancia euclidiana
-- da lista de conjuntos e classifica pelo valor da distancia, o retorno é uma lista
-- de conjunto de teste classificado [((conjunto de teste, classificacao)]
nearNeighbour :: [([Double],String)] -> [([Double],String)] -> [(([Double],String),String)]
nearNeighbour training test = 
    [attribClass object training (computeDist (fst object) (fst (head training))) name | object <- test ]
            where 
                name = snd(head training)

-- funcao que recebe um conjunto e uma lista de conjunto de treino e uma distacia e um nome
-- e atribui a esse conjunto uma classe (classificada pelo algoritimo da distancia do vizinho mais proximo)
-- e retorna um conjunto com uma classificacao 
attribClass :: ([Double],String) -> [([Double],String)] -> Double -> String -> (([Double],String),String)
attribClass object [] _ name = (object,name)
attribClass object (x:xs) distEuclid name = if bigger
                          then attribClass object xs (computeDist (fst object) (fst x)) (snd x)
                          else attribClass object xs distEuclid name
                             where
                                 bigger = big (computeDist (fst object) (fst x)) distEuclid 

-- funcao que recebe dois numeros inteiros e retona a porcentagem dele em Float 
nNeighbourPercent :: Int -> Int -> Float
nNeighbourPercent x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

-- funcao que verifica se um valor e maior que o outro, para o calculo
--do algoritimo de vizinho mais proximo, e retorna
-- um False para caso que seja, e um true para caso seja menor
big :: Ord a => a -> a -> Bool
big x y | x > y = False 
        | otherwise = True

-- funcao que recebe duas listas de coordenadas e calcula
-- a distancia entre elas, pegando cada indice dos vetores e calculando a distancia
-- euclidiana e tirando a raiz para obter um Double 
computeDist :: [Double] -> [Double] -> Double
computeDist object1 object2 = sqrt $ euclid
                  where euclid = euclidAlgorth object1 object2

-- funcao que recebe duas listas de coordenadas e calcula a distancia
-- percorrendo cada indice do vetor e fazendo de acordo com a formula 
-- ((xi1 - xj1)^2 + ..... ate o final das coordenadas e retorna
-- o valor somado e a funcao que a chama tira a raiz
euclidAlgorth :: [Double] -> [Double] -> Double
euclidAlgorth [] [] = 0
euclidAlgorth (obj1:object1) (obj2:object2) = ((obj1 - obj2)^2) + euclidAlgorth object1 object2
