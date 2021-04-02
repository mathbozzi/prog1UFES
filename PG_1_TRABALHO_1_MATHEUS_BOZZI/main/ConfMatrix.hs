module ConfMatrix where
import Data.List (transpose, intercalate)

-- funcao que recebe um conjunto de teste e pega cada nome da classe
-- e gera assim uma lista de Strings com os nomes das classes 
confMatrix :: [([Double],String)] -> [String]
confMatrix list = [(snd objct) | objct <- list ]

-- funcao que recebe um conjunto de teste classificado e pega cada nome da classe
-- da parte classificada pelo algoritimo e gera assim uma lista de Strings com os nomes das classes 
matrixAux :: [(([Double],String),String)] -> [String]
matrixAux list = [ snd i | i <- list]

-- funcao que recebe uma lista de lista de inteiros e monta uma string
-- exemplo: [[1,2,3],[2,3,4]] = "123234" 
joinMatrix:: [[Int]] -> String
joinMatrix list = concat (map show(concat list))

-- funcao que recebe 3 lista de string no qual a primeira sao as classes do arquivo de entrada(sem repeticoes)
-- e as outras duas sao a lista de string de teste e a lista de classificadas pelos algoritimos
-- e assim valia a matriz de confusao verificando pelos dados reais e dados previstos
-- e retorna uma lista de lista de inteiros com os numero de ocorrencias de cada caso
confTable:: [String] -> [String] -> [String] -> [[Int]]
confTable classifiers listTest tryList = 
            [[availableConfMatrix l1 l2 class1 class2 | class1 <- classifiers ] | class2 <- classifiers ]
               where
                   l1 = listTest
                   l2 = tryList

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
