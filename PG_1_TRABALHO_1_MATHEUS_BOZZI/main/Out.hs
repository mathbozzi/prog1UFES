module Out where
import System.IO

-- funcao para escrita do arquivo de saida, onde recebe todos os argumentos
-- e identa em forma de string para a saida em formato esperado
out :: String -> String -> Int -> Int -> [Int] -> Float -> Float -> String -> String -> IO ()
out l1 l2 l3 l4 l5 l6 l7 l8 l9 = do 
    let out1 = "Forneca o nome do arquivo de entrada: "  ++ l1
    let out2 = "Forneca o nome do arquivo de saida: "  ++ l2          
    let out3 = "Forneca o percentual de exemplos de teste: "  ++ (show l3)         
    let out4 = "Forneca o valor da semente para geracao randomizada: "  ++ (show l4)          
    let out5 = "Indices de exemplos de teste: "  ++ (show l5)          
    let out6 = "Acuracia(vizinho): "  ++ (show l6)          
    let out7 = "Acuracia(centroide): "  ++ (show l7)          
    let out8 = "vizinho mais próximo: \n"  ++ l8          
    let out9 = "centroides: \n" ++ l9          
    writeFile l2 (out1 ++ "\n" ++ out2 ++ "\n" ++ out3 ++ "\n" ++ out4 ++ "\n\n" ++ out5 ++ "\n\n" ++ out6 ++ "%\n" ++ out7 ++ "%\n\n" ++ out8 ++ "\n\n" ++ out9)