-- Aluno: Matheus de Abreu Bozzi 
-- Disciplina: Programacao I
-- Segundo trabalho

import Helps
import Random
import Sets
import NearestNeighbour
import K_NearestNeighbour
import Kmeans
import ConfMatrix
import Out
import Folds
import Standardize
import Text.Printf

main :: IO ()
main = do
    putStr "Forneca o nome do arquivo de entrada:"                 --{                        
    filename <- getLine                                            --
    putStr "Forneca o nome do arquivo de saida:"                   --
    outfilename <- getLine                                         --Parte de leitura da entrada padrao
    putStr "Forneca o numero de folds:"                            -- 
    nFolds <- readInt                                              --
    putStr "Forneca o numero de vizinhos:"                         -- 
    nNeigh <- readInt                                              --
    putStr "Forneca o valor da semente para geracao randomizada:"  -- 
    seedRandom <- readInt                                          --
    csvFile <- readFile filename                                   --}

    let objectsList = insertList csvFile                              -- leitura do arquivo CSV
    let objectsTuple = insertTuple $ objectsList                      -- Insercao das estruturas em tupla
    let sizeList = size objectsList                                   -- quantidade de linhas arquivo de entrada
    let randonSet = randomSet sizeList sizeList seedRandom             -- gerador de lista de numeros aleatorios
    let nInt = (truncate(fromIntegral(sizeList)/fromIntegral(nFolds)))  -- divisao da quantidade de folds
    let remnant = ((sizeList) `mod` nFolds)                            -- resto da divisao para separacao dos folds 
        
    let foldsList = unionFolds (breakFolds nInt randonSet) nFolds remnant --monta os N folds
    let sizeFoldsList = sizeFold foldsList                          -- pega o tamanho da lista de folds                       
    let (trainingSet, testSet) = difierSet foldsList objectsTuple   -- separando conjuntos de treino e de teste

    --PADRONIZACAO 
    let trainingSetStandart = standardize trainingSet               --monta varias listas de colunas das coordenadas
    let avarageStandart = standardizeAverage trainingSetStandart    -- calcula a media das colunas para cada fold
    let stdDerStandart = standardizeStdDev trainingSetStandart avarageStandart -- calcula o desvio padrao pra cada fold 
    let standardizeTrainingSet = z_scoreSet avarageStandart stdDerStandart trainingSet -- padroniza o conjunto de treino (z-score)
    let standardizeTestSet = z_scoreSet avarageStandart stdDerStandart testSet --padrozina o conjunto de teste(z-score)
    

    --NEAREST NEIGHBOUR ALGORITHM
    --let nNeighbour = nearNeighbour trainingSet testSet                     -- algoritmo de vizinho mais proximo para cada fold (nao padronizado)
    let nNeighbour = nearNeighbour standardizeTrainingSet standardizeTestSet -- algoritmo de vizinho mais proximo para cada fold (padronizado)
    let nNaccuracy = accuracyNeighbour nNeighbour 0                          -- calcula quantidade de acertos de cada fold
    let nNeighbourAccuracy = nNeighbourPercent (nNaccuracy) (sizeFoldsList)   -- calcula acuracia do algoritimo do vizinho mais proximo para cada fold
    let nNeighbourAccuracyFolds = nNeighbourPercentFold nNeighbourAccuracy (size nNeighbour) -- faz a media da acuracia dos fold
    printf "Acuracia(vizinho): %.2f" (nNeighbourAccuracyFolds::Float)
    printf "%%\n"
    let stDevNNeighbour = standartDeviation nNeighbourAccuracy nNeighbourAccuracyFolds    -- calcula o desvio padrao dos folds
    printf "Desvio-Padrao(vizinho): %.2f" (stDevNNeighbour::Float)
    printf "%%\n"


    -- --K-MEANS ALGORITHM
    let nClassifiers = verifyClass objectsTuple                         -- verifica as classes existentes
    --let coordKmeans = kMeansCoord trainingSet nClassifiers            -- calcula o centro de massa das classes(nao padronizado) para cada fold
    let coordKmeans = kMeansCoord standardizeTrainingSet nClassifiers   -- calcula o centro de massa das classes (padronizado) para cada fold
    let joinCoordKmeans = joinCoordKmeansFold coordKmeans nClassifiers  -- junta o centro de massa com a respectiva classe para cada fold
    -- let nkMeans = nearNeighbour joinCoordKmeans testSet              -- compara no algoritimo de vizinho mais proximo e classifica, para cada fold (nao padronizado)
    let nkMeans = nearNeighbour joinCoordKmeans standardizeTestSet      -- compara no algoritimo de vizinho mais proximo e classifica, para cada fold (padronizado)
    let nCaccuracy = accuracyNeighbour nkMeans 0                        -- calcula a quantidade de acertos para cada fold
    let nkMeansAccuracy = nNeighbourPercent (nCaccuracy) (sizeFoldsList) -- calcula a acuracia do algoritimo de centroide para cada fold
    let nKmeansAccuracyFolds = nNeighbourPercentFold nkMeansAccuracy (size nNeighbour) -- faz a media da acuracia dos folds 
    printf "Acuracia(centroide): %.2f" (nKmeansAccuracyFolds::Float)
    printf "%%\n"
    let stDevKmeans = standartDeviation nkMeansAccuracy nKmeansAccuracyFolds    -- calcula o desvio padrao dos folds
    printf "Desvio-Padrao(centroide): %.2f" (stDevKmeans::Float)
    printf "%%\n"
    

    -- K-NEAREST NEIGHBOUR ALGORITHM
    --let k_nearNeigh = k_NearNeighbour trainingSet testSet nNeigh                      -- algoritimo K-vizinhos mais proximos para cada fold (nao padronizado)
    let k_nearNeigh = k_NearNeighbour standardizeTrainingSet standardizeTestSet nNeigh  -- algoritimo K-vizinhos mais proximos para cada fold (padronizado)
    let kNaccuracy = accuracyNeighbour k_nearNeigh 0                                    -- calcula a quantidade de acertos para cada fold
    let kNeighbourAccuracy = nNeighbourPercent (kNaccuracy) (sizeFoldsList)             -- calcula a acuracia do algoritimo K- vizinhos para cada fold
    let kNeighbourAccuracyFold = nNeighbourPercentFold kNeighbourAccuracy (size nNeighbour) -- faz a media da acuracia dos fold
    printf "Acuracia(k-vizinhos): %.2f" (kNeighbourAccuracyFold::Float)
    printf "%%\n"
    let stDevKNNeighbour = standartDeviation kNeighbourAccuracy kNeighbourAccuracyFold     -- calcula o desvio padrao dos folds
    printf "Desvio-Padrao(k-vizinhos): %.2f" (stDevKNNeighbour::Float)
    printf "%%\n"


    --CONFUSION MATRIX NEAREST NEIGHBOUR
    let cMatrix = confMatrix standardizeTestSet                     -- pega os nomes das classes do conjunto de teste          
    let nNmatrix = matrixAux nNeighbour                             -- pega os nomes das classes classificadas pelo algoritimo vizinho mais proximo
    let indicesNNMatrix = confTable nClassifiers cMatrix nNmatrix   -- compara as classes de teste e classificadas e verifica os erros para cada fold                 
    let joinMatrixNeighbour = joinMatrix ((size nClassifiers)-1) indicesNNMatrix --junta as listas por indices iguais 
    let joinMatrixNNFold = mountConfMatrix joinMatrixNeighbour        -- soma todos os erros e coloca em formato de matriz 
    let nNeighbourMatrix = stringMatrix joinMatrixNNFold              -- pega a matriz, os erros e formata para string
    --putStrLn nNeighbourMatrix


    --CONFUSION MATRIX K-MEANS
    let nKMmatrix = matrixAux nkMeans                               -- pega os nomes das classes classificadas pelo algoritimo  de centroide 
    let indicesKMMatrix = confTable nClassifiers cMatrix nKMmatrix  -- compara as classes de teste e classificadas e verifica os erros e coloca na tabela                    
    let joinMatrixKmeans = joinMatrix ((size nClassifiers)-1) indicesKMMatrix  --junta as listas por indices iguais 
    let joinMatrixKMFold = mountConfMatrix joinMatrixKmeans         -- soma todos os erros e coloca em formato de matriz 
    let kMeansMatrix = stringMatrix joinMatrixKMFold                -- pega a matriz e formata os erros para string
    --putStrLn kMeansMatrix


    -- CONFUSION MATRIX K-NEAREST NEIGHBOUR ALGORITHM
    let nKNmatrix = matrixAux k_nearNeigh                             -- pega os nomes das classes classificadas pelo algoritimo K-vizinho mais proximo
    let indicesKNNMatrix = confTable nClassifiers cMatrix nKNmatrix   -- compara as classes de teste e classificadas e verifica os erros para cada fold        
    let joinMatrixKNeighbour = joinMatrix ((size nClassifiers)-1) indicesKNNMatrix --junta as listas por indices iguais 
    let joinMatrixKNNFold = mountConfMatrix joinMatrixKNeighbour      -- soma todos os erros e coloca em formato de matriz 
    let kNNeighbourMatrix = stringMatrix joinMatrixKNNFold            -- pega a matriz e formata os erros para string          
    --putStrLn kNNeighbourMatrix

    -- --PROGRAM OUT -- escreve no arquivo de saida
    out outfilename nNeighbourMatrix kMeansMatrix kNNeighbourMatrix

