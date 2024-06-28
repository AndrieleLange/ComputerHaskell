import System.IO 

type ListMemoria = [(Int, Int)] -- lista de memorias

data CPU = CPU { acc :: Int, pc :: Int, memoria :: ListMemoria, eqz :: Int } deriving (Show)

-- Cria CPU
criaCPU :: Int -> Int -> ListMemoria -> CPU
criaCPU acc pc memoria = CPU { acc = acc, pc = pc, memoria = memoria, eqz = eqz }

-- Escrever na memória
salvaMemoria :: ListMemoria -> Int -> Int -> ListMemoria
salvaMemoria [] endereco conteudo = [(endereco, conteudo)]
salvaMemoria ((e, c):xs) endereco conteudo
    | e == endereco = (e, conteudo) : xs
    | otherwise     = (e, c) : salvaMemoria xs endereco conteudo

-- Ler a memória
readMem :: ListMemoria -> Int -> Int
readMem [] _ = 0
readMem ((e, c):xs) endereco
    | e == endereco = c
    | otherwise     = readMem xs endereco


-- Atualizar conteúdo na memória
atualizaMemoria :: Int -> Int -> ListMemoria -> ListMemoria
atualizaMemoria endereco conteudo memoria = salvaMemoria memoria endereco conteudo


-- instrução LOD
-- execLOD (endereço, memória, acc, eqz) -> (endereço, memória, acc, eqz)
execLOD :: Int -> CPU -> CPU
execLOD end cpu = cpu {acc = readMem (memoria cpu) end}


-- instrução STO
-- execSTO end (ListMemoria, acc, eqz) -> (ListMemoria, acc, eqz)
execSTO :: Int -> CPU -> CPU
execSTO end cpu = cpu {memoria = atualizaMemoria end (acc cpu) (acc mem)}

-- instrução CPE
-- execCPE (endereço, memória, acc, eqz) -> (endereço, memória, acc, eqz)
execCPE :: Int -> CPU -> CPU
execCPE end cpu = cpu {
    eqz = if readMem memoria end == acc
    then 0
    else 1
}

-- Instrução ADD
execADD :: Int -> CPU -> CPU
execADD end cpu = cpu{ acc = newAcc}
    where 
        val = readMem mem end
        newAcc = val + acc

-- instrução SUB
execSUB :: Int -> CPU -> CPU
execSUB end cpu = cpu {acc = newAcc}
    where 
        val = readMem mem end
        newAcc = acc - val


-- instrução NOP
-- execNOP(mem,acc,eqz) = (mem,acc,eqz)
execNOP :: CPU -> CPU 
execNOP cpu = cpu

-- instrução HLT
execHLT :: CPU -> CPU 
execHLT cpu = cpu {pc = -1}


-- Executar um programa
-- recebe uma memória e devolve uma memória
executar :: ListMememoria -> ListMememoria
executar mem = 

-- ler o arquivo e devolver a lista
-- ciclo busca, decodifica e executa
-- faz até encontrar o NOP
-- criar uma função de cada instrução



-- Instrução JMP
execJMP Int -> CPU -> CPU
execJMP end cpu = cpu { pc = readMem mem end}

-- instrução JMZ
execJMZ Int -> CPU -> CPU
execJMZ end cpu = 
    if acc cpu == 0
        then execJMP end cpu
        else cpu



-- Executar um programa
executar :: CPU -> CPU
executar cpu
    | pc cpu < 0 = cpu -- Se pc for -1, o programa parou
    | otherwise =
        let instr = readMem (memoria cpu) (pc cpu)
            end = readMem (memoria cpu) (pc cpu + 1)
            newCPU = case instr of
                2  -> execLOD end cpu
                4  -> execSTO end cpu
                6  -> execJMP end cpu
                8  -> execJMZ end cpu
                10 -> execCPE end cpu
                14 -> execADD end cpu
                16 -> execSUB end cpu
                18 -> execNOP cpu
                20 -> execHLT cpu
                _  -> cpu -- Instrução desconhecida, não faz nada
        in traceCPU newCPU { pc = pc newCPU + 2 }
  where
    traceCPU cpu = cpu { memoria = trace (showCPU cpu) (memoria cpu) }
    showCPU cpu = "CPU { acc: " ++ show (acc cpu) ++ ", pc: " ++ show (pc cpu) ++ ", eqz: " ++ show (eqz cpu) ++ " }"

-- Leitura da memória de um arquivo
carregaMemoria :: FilePath -> IO ListMemoria
carregaMemoria caminho = do
    conteudo <- readFile caminho
    return (map (\linha -> let [end, val] = map read (words linha) in (end, val)) (lines conteudo))

main :: IO ()
main = do
    memoria <- carregaMemoria "sub.txt"
    let cpu = criaCPU 0 0 memoria
    let cpuFinal = executar cpu
    print cpuFinal