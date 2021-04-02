-- Aluno: Matheus de Abreu Bozzi 
-- Disciplina: Programacao I

import Helps
import Random
import Sets
import NearestNeighbour
import Kmeans
import ConfMatrix
import Out

main :: IO ()
main = do
    putStr "Forneca o nome do arquivo de entrada:"                 --{                        
    filename <- getLine                                            --
    putStr "Forneca o nome do arquivo de saida:"                   --
    outfilename <- getLine                                         --Parte de leitura da entrada padrao
    putStr "Forneca o percentual de exemplos de teste:"            -- 
    percent <- readInt                                             --
    putStr "Forneca o valor da semente para geracao randomizada:"  -- 
    seedRandom <- readInt                                          --
    csvFile <- readFile filename                                   --}

    let objectsList = insertList csvFile                           -- leitura do arquivo CSV
    let objectsTuple = insertTuple $ objectsList                   -- Insercao das estruturas em tupla
    --print(objectsTuple)
    let sizeList = size objectsList                                -- quantidade de linhas arquivo de entrada
    -- print(sizeList)
    let nteste = truncate((fromIntegral(percent)/100.0)*fromIntegral(sizeList)) --funcao nTeste : qtd de elementos a serem testados
    let randonSet = randomSet nteste sizeList seedRandom          -- gerador de lista de numeros aleatorios
    --print(randonSet)
    let (trainingSet, testSet) = difierSet randonSet objectsTuple   -- separando conjuntos de treino e de teste
    -- print(trainingSet)
    --print(testSet)
    
    --NEAREST NEIGHBOUR ALGORITHM
    let nNeighbour = nearNeighbour trainingSet testSet              -- algoritmo de vizinho mais proximo
    --print(nNeighbour)
    let nNaccuracy = accuracyNeighbour nNeighbour 0                 -- calcula quantidade de acertos
    -- print(nNaccuracy)
    -- print(size nNeighbour)
    let nNeighbourAccuracy = nNeighbourPercent nNaccuracy (size nNeighbour) -- calcula acuracia do algoritimo do vizinho mais proximo
    --print(nNeighbourAccuracy)
    
    --K-MEANS ALGORITHM
    let nClassifiers = verifyClass objectsTuple                     -- verifica as classes existentes
    --print(nClassifiers)
    let coordKmeans = kMeansCoord trainingSet nClassifiers          -- calcula o centro de massa das classes
    --print(coordKmeans)
    let joinCoordKmeans = zip coordKmeans nClassifiers              -- junta o centro de massa com a respectiva classe
    -- print(joinCoordKmeans)
    let nkMeans = nearNeighbour joinCoordKmeans testSet             -- compara no algoritimo de vizinho mais proximo
    --print(nkMeans)
    let nCaccuracy = accuracyNeighbour nkMeans 0                    -- calcula a quantidade de acertos
    let nkMeansAccuracy = nNeighbourPercent nCaccuracy (size nkMeans)   -- calcula a acuracia do algoritimo de centroide
    --print(nkMeansAccuracy)

    --CONFUSION MATRIX NEAREST NEIGHBOUR
    let cMatrix = confMatrix testSet                                -- pega os nomes das classes do conjunto de teste          
    --print(cMatrix)
    let nNmatrix = matrixAux nNeighbour                             -- pega os nomes das classes classificadas pelo algoritimo vizinho mais proximo
    --print(nNmatrix)
    let indicesNNMatrix = confTable nClassifiers cMatrix nNmatrix   -- compara as classes de teste e classificadas e verifica os erros                    
    --print(indicesNNMatrix)
    -- let joinTexteMatrix = joinMatrix indicesNNMatrix
    -- print(joinTexteMatrix)
    let nNeighbourMatrix = stringMatrix indicesNNMatrix             -- pega a matriz, os erros e formata para string
    --putStrLn nNeighbourMatrix

    --CONFUSION MATRIX K-MEANS
    let nKMmatrix = matrixAux nkMeans                               -- pega os nomes das classes classificadas pelo algoritimo  de centroide 
    -- print(cMatrix)
    --print(nKMmatrix)
    let indicesKMMatrix = confTable nClassifiers cMatrix nKMmatrix  -- compara as classes de teste e classificadas e verifica os erros e coloca na tabela                    
    --print(indicesKMMatrix)
    let kMeansMatrix = stringMatrix indicesKMMatrix                 -- pega a matriz e formata os erros para string
    --putStrLn kMeansMatrix

    --PROGRAM OUT -- escreve no arquivo de saida
    out filename outfilename percent seedRandom randonSet nNeighbourAccuracy nkMeansAccuracy nNeighbourMatrix kMeansMatrix








    

    
