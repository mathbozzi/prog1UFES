module ConfMatrix where
import Data.List (transpose, intercalate)

-- funcao que recebe um conjunto de teste e pega cada nome da classe
-- e gera assim uma lista de Strings com os nomes das classes 
confMatrixHelp :: [([Double],String)] -> [String]
confMatrixHelp list = [(snd objct) | objct <- list ]


confMatrix :: [[([Double],String)]] -> [[String]]
confMatrix list = [ confMatrixHelp objct | objct <- list ]


-- funcao que recebe um conjunto de teste classificado e pega cada nome da classe
-- da parte classificada pelo algoritimo e gera assim uma lista de Strings com os nomes das classes 
matrixAuxHelp :: [(([Double],String),String)] -> [String]
matrixAuxHelp list = [ snd i | i <- list]

matrixAux :: [[(([Double],String),String)]] -> [[String]]
matrixAux list = [ matrixAuxHelp i | i <- list]


-- funcao que recebe 3 lista de string no qual a primeira sao as classes do arquivo de entrada(sem repeticoes)
-- e as outras duas sao a lista de string de teste e a lista de classificadas pelos algoritimos
-- e assim valia a matriz de confusao verificando pelos dados reais e dados previstos
-- e retorna uma lista de lista de inteiros com os numero de ocorrencias de cada caso
confTableHelp:: [String] -> [String] -> [String] -> [[Int]]
confTableHelp classifiers listTest tryList = 
            [[availableConfMatrix l1 l2 class1 class2 | class1 <- classifiers ] | class2 <- classifiers ]
               where
                   l1 = listTest
                   l2 = tryList


confTable:: [String] -> [[String]] -> [[String]] -> [[[Int]]]
confTable classifiers (x:[]) (y:[]) = [confTableHelp classifiers x y]  
confTable classifiers (x:xs) (y:ys) = [confTableHelp classifiers x y] ++ confTable classifiers xs ys

-- funcao que avalia duas categorias de classes e vai somando as ocorrencias
-- das classes com nomes iguais e no final gera um valor inteiro com o 
-- numero de ocorrencias dos casos 
availableConfMatrix :: [String] -> [String] -> String -> String -> Int
availableConfMatrix listTest tryList class1 class2 = 
            sum [ 1 | (firstN, sndN) <- (zip listTest tryList), firstN == class1, sndN == class2]

-- funcao que recebe uma lista de lista de inteiros com o numero de ocoorencias
-- da matriz de confucao e monta a matriz identada para a saida padrao, essa funcao
-- retorna uma String no qual sera feita a escrita no arquivo de saida, separada por virgula e 
-- com quebra de linhas ao final de cada lista de inteiros passada
-- exemplo: [[1,2],[3,4]] = "  1,  2\n  3,  4\n" 
stringMatrix:: [[Int]] -> String
stringMatrix = unTable ", " . equalizeCellLengths . (map . map) show

--funcao que ajuda a somar elementos de listas
--recebe uma lista de listas de inteiros e soma cada elemento
-- com a funcao zipWith(+) e usando recursao ate a condicao de parada de n ter mais calda []
-- e retorna assim uma lista de inteiros somados 
mountConfMatrixHelp :: [[Int]] -> [Int] 
mountConfMatrixHelp (x:[]) = x
mountConfMatrixHelp (x:xs) = zipWith (+) x $ mountConfMatrixHelp xs

--funcao que recebe uma lista de lista de lista de inteiros 
-- e percorre a lista de listas em cada elemento para assim ir somando
-- cada lista e retornar uma lista de listas somadas
-- exemplo [[[10,1,1],[13,2,0]],[[1,8,4],[0,15,2]],[[1,2,8],[0,1,3]]]
-- saida [[23,3,1],[1,23,6],[1,3,11]]
mountConfMatrix :: [[[Int]]] -> [[Int]]
mountConfMatrix xs = [ mountConfMatrixHelp x | x <- xs]

--funcao que recebe uma lista de lista de lista de inteiros e um inteiro
-- exemplo [[[10,1,1],[1,8,4],[1,2,8]],[[13,2,0],[0,15,2],[0,1,3]]]
-- e junta as listas por indices, os que sao dos mesmos indices, e retorna
--exemplo [[[10,1,1],[13,2,0]],[[1,8,4],[0,15,2]],[[1,2,8],[0,1,3]]]
joinMatrix :: Int -> [[[Int]]] -> [[[Int]]]
joinMatrix xi xs = [[ x !! i |  x <- xs] | i <- [0..xi]]


-- funcao que recebe uma string e identa os valores e no final de cada linha 
-- insere um \n
unTable :: String -> [[String]] -> String
unTable colSpacer = intercalate "\n" . map (intercalate colSpacer)

-- funcao que encontra o maior elemento da coordenada e retorna o tamanho para 
-- a formatacao dos elementos da matriz de confusao
maxLineLength :: [[a]] -> Int
maxLineLength = maximum . map length

-- funcao que calcula o comprimento da entrada mais longa de 
-- cada coluna da matriz, para identacao
maxCellLengthsPerColumn :: [[[a]]] -> [Int]
maxCellLengthsPerColumn = map maxLineLength . transpose

-- funcao que formata a matriz
equalizeCellLengths :: [[String]] -> [[String]]
equalizeCellLengths table = map (equalize maxLengths) table
    where
        maxLengths = maxCellLengthsPerColumn table
        equalize = flip $ zipWith (padToLengthLeft ' ')

-- funcao que insere uma lista até um determinado comprimento à direita.
--  exemplo "12" 5
--   "12___"
--  exemplo "123" 4
--   "123_"
padToLengthLeft :: a -> [a] -> Int -> [a]
padToLengthLeft padding list n = (replicate (n - length list) padding) ++ list
