import System.IO
import Debug.Trace

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
