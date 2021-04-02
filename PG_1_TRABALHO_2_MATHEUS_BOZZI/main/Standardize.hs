module Standardize where
import Data.List

-- funcao que recebe os folds de treino e retorna os valores
-- das colunas das coordenadas em uma lista de lista de lista de double 
-- essa funcao percorre cada fold para retornar em uma lista
standardize :: [[([Double],String)]] -> [[[Double]]]
standardize (x:[]) = [transpose (standardizeHelp x)]
standardize (x:xs) = [transpose (standardizeHelp x)] ++ standardize xs

-- funcao que recebe um fold de treino e retorna uma lista de 
-- lista de colunas das coordenadas de cada fold
standardizeHelp ::[([Double],String)] -> [[Double]]
standardizeHelp list = [fst l | l <- list]

-- funcao que recebe uma lista de lista de lista de doubles e 
-- calcula a media das colunas, retornando assim uma lista de 
-- lista de doubles com as medias em cada fold 
standardizeAverage :: [[[Double]]] -> [[Double]]
standardizeAverage (x:[]) = [standardizeAverageHelp x] 
standardizeAverage (x:xs) = [standardizeAverageHelp x] ++ standardizeAverage xs

-- funcao que recebe uma lista de lista de doubles com as coordenadas
-- das colunas de cada fold separado e retona uma lista de double
-- com a media de cada coluna dos dados lidos 
standardizeAverageHelp :: [[Double]] -> [Double]
standardizeAverageHelp (x:[]) = [avarage x]
standardizeAverageHelp (x:xs) = [avarage x] ++ standardizeAverageHelp xs

--funcao que calcula a media 
avarage :: Fractional a => [a] -> a
avarage ns = sum ns / fromIntegral(length ns)

-- funcao que recebe uma lista de lista de lista de coordenadas (colunas de treino) 
-- e uma lista de medias de cada fold coluna e retorna uma lista de lista
-- contendo o desvio padrao de cada coluna das coordenadas para todos os folds
standardizeStdDev :: [[[Double]]] -> [[Double]] -> [[Double]]
standardizeStdDev (x:[]) (y:[]) = [standardizeStdDevHelp x y] 
standardizeStdDev (x:xs) (y:ys) = [standardizeStdDevHelp x y] ++ standardizeStdDev xs ys

-- funcao que recebe uma lista de lista de coordenadas e uma lista de medias
-- e calcula o desvio padrao em cada fold separado 
standardizeStdDevHelp :: [[Double]] -> [Double] -> [Double]
standardizeStdDevHelp (x:[]) (y:[]) = [standartDev x y]
standardizeStdDevHelp (x:xs) (y:ys) = [standartDev x y] ++ standardizeStdDevHelp xs ys

-- funcao que recebe uma lista de coordenadas e a media, e calcula o desvio padrao
standartDev :: [Double] -> Double -> Double
standartDev xs avr = sqrt . average . map ((^2) . (-) avr) $ xs
                    where average = (/) <$> sum <*> realToFrac . length

-- funcao que recebe uma lista de lista de media, desvio padrao e K folds de conjunto
-- de treino, para assim calcular o z-score de cada coordenada de cada fold
-- e retornar um conjunto de treino padronizado com a coordenada o valor de z
z_scoreSet :: [[Double]] -> [[Double]] -> [[([Double],String)]] -> [[([Double],String)]]
z_scoreSet (x:[]) (y:[]) (z:[]) = [z_scoreSetHelp x y z]
z_scoreSet (x:xs) (y:ys) (z:zs) = [z_scoreSetHelp x y z] ++ z_scoreSet xs ys zs

-- funcao que recebe uma lista de media e de desvio padrao e um fold de conjunto de
-- treino, e assim calcula o z-score para cada coordenada 
-- e retorna uma lista de tupla de dados padronizados com o valor de z
z_scoreSetHelp :: [Double] -> [Double] -> [([Double],String)] -> [([Double],String)]
z_scoreSetHelp xs ys (z:[]) = [(standartCoord (fst z) xs ys, snd z)]
z_scoreSetHelp xs ys (z:zs) = [(standartCoord (fst z) xs ys, snd z)] ++ z_scoreSetHelp xs ys zs

-- funcao que recebe uma lista de medias e uma lista de desvio padrao
-- e uma lista de coordenadas para o calculo do z-score e 
-- retorna uma lista de coordenadas em z
standartCoord :: [Double] -> [Double] -> [Double] -> [Double]
standartCoord (x:[]) (y:[]) (z:[]) = [zscore x y z]
standartCoord (x:xs) (y:ys) (z:zs) = [zscore x y z] ++ standartCoord xs ys zs

-- funcao que calcula o z-score dado a formula z = (xi - xm )/dp
zscore :: Double -> Double -> Double -> Double
zscore xs media desvio = (xs - media) / desvio 