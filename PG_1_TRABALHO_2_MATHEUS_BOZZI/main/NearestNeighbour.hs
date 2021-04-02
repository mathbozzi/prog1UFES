module NearestNeighbour where

-- funcao que recebe um conjunto do algoritimo de vizinho mais proximo,
-- a sua classificacao resultante e um inteiro que é a soma,
-- retornando assim um inteiro com a quantidade de acertos 
accuracyNeighbourHelp:: [(([Double],String),String)] -> Int -> Int
accuracyNeighbourHelp [] cont = cont
accuracyNeighbourHelp nNeighbour cont = if equals (snd (fst (head nNeighbour))) (snd (head nNeighbour))
                                    then accuracyNeighbourHelp (tail nNeighbour) cont+1
                                    else accuracyNeighbourHelp (tail nNeighbour) cont

-- funcao que recebe os folds e itera sobre eles
-- para calcular o numero de erros das classificacoes 
-- e retorna uma lista de inteiros com os erros de cada fold para 
-- o calculo da acuracia, ela usa a funcao accuracyNeighbourHelp 
-- para calcular os erros de cada fold separados
accuracyNeighbour:: [[(([Double],String),String)]] -> Int -> [Int]
accuracyNeighbour list cont = [accuracyNeighbourHelp x cont | x <- list] 


-- funcao que recebe uma lista de porcentagens dos folds a qtd de folds
-- e calcula a media e retorna um float para o calculo da acuracia 
nNeighbourPercentFold :: [Float] -> Int -> Float
nNeighbourPercentFold list nFolds = (sum list) / (fromIntegral nFolds ::Float)


-- funcao que verifica se duas strings são iguais e 
-- retorna um Booleano True para caso afirmativo e False para caso negativo
equals:: String -> String -> Bool
equals name1 name2 = if name1 == name2
                     then True
                     else False

-- funcao que recebe dois conjuntos, o de treino e o de teste, verifica a distancia euclidiana
-- da lista de conjuntos e classifica pelo valor da distancia, o retorno é uma lista
-- de conjunto de teste classificado [((conjunto de teste, classificacao)]
nearNeighbourHelp :: [([Double],String)] -> [([Double],String)] -> [(([Double],String),String)]
nearNeighbourHelp training test = 
    [attribClass object training (computeDist (fst object) (fst (head training))) name | object <- test ]
            where 
                name = snd(head training)

-- nearNeighbour :: [[([Double],String)]] -> [[([Double],String)]] -> [[(([Double],String),String)]]
-- nearNeighbour training test = [nearNeighbourHelp x y | x <- training, y <- test]
nearNeighbour :: [[([Double],String)]] -> [[([Double],String)]] -> [[(([Double],String),String)]]
nearNeighbour (x:[]) (y:[]) = [nearNeighbourHelp x y] 
nearNeighbour (x:xs) (y:ys) = [nearNeighbourHelp x y] ++ nearNeighbour xs ys


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


-- funcao que recebe um conjunto e uma lista de conjunto de treino e uma distacia e um segundo conjunto
-- e atribui a esse conjunto uma classe (classificada pelo algoritimo da distancia do vizinho mais proximo)
-- e retorna um conjunto 
attribClassHelp :: ([Double],String) -> [([Double],String)] -> Double -> ([Double],String) -> ([Double],String)
attribClassHelp _ [] distEuclid sndList = sndList
attribClassHelp fstList (x:xs) distEuclid sndList = if euclidAlgorth (fst fstList) (fst x) < distEuclid
                                                    then attribClassHelp fstList xs (euclidAlgorth (fst fstList) (fst x)) x
                                                    else attribClassHelp fstList xs distEuclid sndList


-- funcao que recebe dois numeros inteiros e retona a porcentagem dele em Float 
nNeighbourPercentHelp :: Int -> Int -> Float
nNeighbourPercentHelp x y =   100 * ( a / b )
  where a = fromIntegral x :: Float
        b = fromIntegral y :: Float

-- funcao que recebe uma lista da quantidade de acertos (int) de cada fold e
-- recebe tambem o tamanho de cada fold e itera sobre cada fold para calcular
-- a procentagem na funcao nNeighbourPercentHelp e retorna uma lista
-- com as porcentagens de cada fold
nNeighbourPercent :: [Int] -> [Int] -> [Float]
nNeighbourPercent (x:[]) (y:[]) = [nNeighbourPercentHelp x y]
nNeighbourPercent (x:xs) (y:ys) = [nNeighbourPercentHelp x y] ++ nNeighbourPercent xs ys


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
