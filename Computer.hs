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
readMem (x:xs) e
    | e == fst x = snd x
    | e /= fst x = readMem xs e


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
execSTO end cpu = cpu {memoria = atualizaMemoria end (acc mem)}

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
