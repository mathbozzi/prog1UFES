module Helps where 

-- cast de string para double
readDouble :: String -> Double
readDouble = read

-- cast para Int da entrada padrao
readInt :: IO Int
readInt = fmap read getLine

-- funcao que pega o ultimo elemento da lista
lastF :: [a] -> a
lastF [x] = x
lastF (_:xs) = lastF xs

-- funcao que retorna o inicio de uma lista com excessao do ultimo elemento
initFunc:: [a] -> [a]
initFunc [] = []
initFunc [x] = []
initFunc (x:xs) = x: initFunc xs

-- funcao que realiza a leitura do arquivo de entrada, le linha por linha separando por virgula
insertList csv = map (split) $ lines csv

-- funcao que separa os elementos por virgula do arquivo de entrada e coloca em uma lista de strings
split :: String -> [String]
split = splitWords.dropWhile (==',') 
    where
        splitWords "" = []
        splitWords s = 
              let word = takeWhile (/=',') s 
                  (_, rest) = splitAt (length word) s
              in word : splitWords (dropWhile (==',') rest)

--funcao que insere os elementos em tuplas, onde a funcao recebe como parametro 
-- uma lista de lista de strings e retorna em uma lista de tuplas com os elementos separados
-- em uma lista de coordenadas e no nome da classe - Ex: [([1.1,2.3,3.8],"nomeClasse")]
insertTuple :: [[String]] -> [([Double],String)]
insertTuple xs = [(coord x,lastF x)|x<-xs]
                where
                    coord = \x -> map (readDouble) (initFunc x)

-- funcao que conta quantos elementos tem na lista, 
--recebe a lista e o numero 0 para contar e retorna um valor inteiro
cont :: [a] -> Int -> Int
cont xs size = if null xs then size 
                          else cont (tail xs) (size+1)

-- funcao que recebe uma lista e retorna o tamanho da lista
size :: [a] -> Int
size xs = cont xs 0 
