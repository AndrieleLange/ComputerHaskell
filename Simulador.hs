type ListMemoria = [(Int, Int)] -- lista de memorias

data CPU = CPU { acc :: Int, pc :: Int, memoria :: ListMemoria, eqz :: Bool } deriving (Show)

-- Cria CPU
criaCPU :: Int -> Int -> ListMemoria -> CPU
criaCPU acc pc memoria = CPU { acc = acc, pc = pc, memoria = memoria, eqz = False }

-- Escrever na memória
salvaMemoria :: ListMemoria -> Int -> Int -> ListMemoria
salvaMemoria [] endereco conteudo = [(endereco, conteudo)]
salvaMemoria ((e, c):xs) endereco conteudo
    | e == endereco = (e, conteudo) : xs
    | otherwise     = (e, c) : salvaMemoria xs endereco conteudo

-- Ler a memória
readMem :: ListMemoria -> Int -> Int
readMem [] _ = 0 -- Valor padrão se o endereço não existir
readMem ((e, c):xs) endereco
    | e == endereco = c
    | otherwise     = readMem xs endereco

-- Atualizar conteúdo na memória
atualizaMemoria :: Int -> Int -> ListMemoria -> ListMemoria
atualizaMemoria endereco conteudo memoria = salvaMemoria memoria endereco conteudo

-- Instrução LOD
execLOD :: Int -> CPU -> CPU
execLOD end cpu = cpu { acc = readMem (memoria cpu) end }

-- Instrução STO
execSTO :: Int -> CPU -> CPU
execSTO end cpu = cpu { memoria = atualizaMemoria end (acc cpu) (memoria cpu) }

-- Instrução JMP
execJMP :: Int -> CPU -> CPU
execJMP end cpu = cpu { pc = end }

-- Instrução JMZ
execJMZ :: Int -> CPU -> CPU
execJMZ end cpu = if acc cpu == 0 then execJMP end cpu else cpu

-- Instrução CPE
execCPE :: Int -> CPU -> CPU
execCPE end cpu =
    let newAcc = if readMem (memoria cpu) end == acc cpu then 0 else 1
    in cpu { acc = newAcc, eqz = (newAcc == 0) }

-- Instrução ADD
execADD :: Int -> CPU -> CPU
execADD end cpu =
    let newAcc = acc cpu + readMem (memoria cpu) end
    in cpu { acc = newAcc, eqz = (newAcc == 0) }

-- Instrução SUB
execSUB :: Int -> CPU -> CPU
execSUB end cpu =
    let newAcc = acc cpu - readMem (memoria cpu) end
    in cpu { acc = newAcc, eqz = (newAcc == 0) }

-- Instrução NOP
execNOP :: CPU -> CPU
execNOP cpu = cpu

-- Instrução HLT
execHLT :: CPU -> CPU
execHLT cpu = cpu { pc = -1 } -- Usamos -1 para indicar que o programa parou

-- Executar um cpu recursivamente
executar :: CPU -> IO CPU
executar cpu
    | pc cpu == -1 = return cpu
    | otherwise    = do
        let pcAtual = pc cpu
        putStrLn $ "Executando instrução na posição " ++ show pcAtual
        let newCPU = executarInstrucao cpu
        putStrLn $ "Estado da CPU após a instrução: " ++ show newCPU
        executar newCPU

-- Executar uma instrução
executarInstrucao :: CPU -> CPU
executarInstrucao cpu =
    let pcAtual = pc cpu
        instrucao = readMem (memoria cpu) pcAtual
        parametro = readMem (memoria cpu) (pcAtual + 1)
    in case instrucao of
        2  -> execLOD parametro cpu { pc = pcAtual + 2 }
        4  -> execSTO parametro cpu { pc = pcAtual + 2 }
        6  -> execJMP parametro cpu { pc = pcAtual + 2 }
        8  -> execJMZ parametro cpu { pc = pcAtual + 2 }
        10 -> execCPE parametro cpu { pc = pcAtual + 2 }
        14 -> execADD parametro cpu { pc = pcAtual + 2 }
        16 -> execSUB parametro cpu { pc = pcAtual + 2 }
        18 -> execNOP cpu { pc = pcAtual + 1 }
        20 -> execHLT cpu { pc = pcAtual + 1 }
        _  -> cpu { pc = pcAtual + 1 } -- Instrução desconhecida, não faz nada

-- Leitura da memória de um arquivo
carregaMemoria :: FilePath -> IO ListMemoria
carregaMemoria caminho = do
    conteudo <- readFile caminho
    return (map (\linha -> let [end, val] = map read (words linha) in (end, val)) (lines conteudo))

-- Função para mostrar o estado final da memória
mostrarMemoriaFinal :: CPU -> IO ()
mostrarMemoriaFinal cpu = do
    putStrLn "Estado final da memória:"
    mapM_ (\(end, val) -> putStrLn $ "Endereço " ++ show end ++ ": " ++ show val) (memoria cpu)

main :: IO ()
main = do
    memoria <- carregaMemoria "somaTeste.txt"
    let cpu = criaCPU 0 0 memoria
    cpuFinal <- executar cpu
    mostrarMemoriaFinal cpuFinal
