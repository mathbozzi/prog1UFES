module Out where
import System.IO

-- funcao para escrita do arquivo de saida, onde recebe todos os argumentos
-- e identa em forma de string para a saida em formato esperado
out :: String -> String -> String -> String -> IO ()
out l0 l1 l2 l3 = do         
    let out1 = "vizinho mais próximo: \n"  ++ l1        
    let out2 = "centroides: \n" ++ l2          
    let out3 = "k-vizinhos mais próximos: \n" ++ l3          
    writeFile l0 (out1 ++ "\n\n" ++ out2 ++ "\n\n"++ out3)