-- O programa começa no 0
type ListMemoria = [(Int, Int)] -- lista de memorias

data CPU = CPU { acc :: Int, pc :: Int, memoria :: ListMemoria } deriving (Show)

-- Cria CPU
criaCPU :: Int -> Int -> ListMemoria -> CPU
criaCPU acc pc memoria = CPU { acc = acc, pc = pc, memoria = memoria, eqz = eqz }


-- Escrever na memória
-- Armazenar o conteúdo em um endereço de memória
-- writeMem(memoria,endereço,conteudo)=memoria
-- Exemplo:
-- wirteMem [(0,10),(1,3),(2,23),(10,100)] 1 6 = [(0,10),(1,6),(2,23),(10,100)]
-- Salvar na memoria
salvaMemoria :: ListMemoria -> Int -> Int -> ListMemoria
salvaMemoria memoria  =  (e, c) : ListMemoria


-- Ler a memória
-- Retornar o conteúdo do endereço de memória
-- readMem(memoria,endereco)=conteudo
-- Exemplo:
-- readMem [(0,10),(1,3),(2,23),(10,100)] 1 = 3
readMem :: ListMemoria -> Int -> Int
readMem (x:xs) e
    | e == fst x = snd x
    | e /= fst x = readMem xs e


-- Atualizar conteúdo na memória
atualizaMemoria :: Int -> Int -> ListMemoria -> ListMemoria
atualizaMemoria _ _ [] = salvaMemoria endereco cont ((e, c):xs)  -- Se a memória estiver vazia, salva
atualizaMemoria endereco cont ((e, c):xs)
    | endereco == e     = (e, cont) : xs  -- Atualiza o valor se o endereço for encontrado
    | otherwise         = atualizaMemoria endereco cont xs  -- Continua procurando

    

-- Tanto os códigos de instrução como o conteúdo dos endereços de memória 
-- poderão ser informados em 
-- decimal, desde que respeitados o tamanho máximo de bits correspondentes 
-- ao valor binário associado.



-- Executar um programa
-- recebe uma memória e devolve uma memória
executar :: ListMememoria -> ListMememoria
executar mem = 

-- ler o arquivo e devolver a lista
-- ciclo busca, decodifica e executa
-- faz até encontrar o NOP
-- criar uma função de cada instrução

-- instrução LOD
-- execLOD (endereço, memória, acc, eqz) -> (endereço, memória, acc, eqz)
execLOD :: Int -> (ListMemoria, Int, Int) -> (ListMemoria, Int, Int)
execLOD end (mem,acc,eqz) = (mem,readMem mem end,eqz)

-- instrução STO
-- execSTO end (ListMemoria, acc, eqz) -> (ListMemoria, acc, eqz)
execSTO :: Int -> (ListMememoria, Int, Int) -> (ListMemoria, Int, Int)
execSTO end (mem, acc, eqz) = (atualizaMemoria end acc mem, acc, eqz)

-- Instrução JMP
execJMP Int -> (ListMememoria, Int, Int) -> (ListMemoria, Int, Int)
execJMP end (mem, acc, eqz) = 
    pc = readMem end mem 




-- instrução JMZ

-- Instrução ADD
execADD :: Int -> (ListMememoria, Int, Int) -> (ListMemoria, Int, Int)
execADD end (mem,acc,eqz) = (mem, newAcc, eqz)
    where 
        val = readMem mem end
        newAcc = val + acc

-- instrução SUB
execSUB :: Int -> (ListMememoria, Int, Int) -> (ListMemoria, Int, Int)
execSUB end (mem,acc,eqz) = (mem, newAcc, eqz)
    where 
        val = readMem mem end
        newAcc = val - acc


-- Instrução HLT

-- instrução CPE
-- execCPE (endereço, memória, acc, eqz) -> (endereço, memória, acc, eqz)
execCPE :: Int -> (ListMemoria, Int, Int) -> (ListMemoria, Int, Int)
execCPE end (mem, acc, eqz) =
    if readMem mem end == acc
    then (mem, 0, eqz)
    else (mem, 1, eqz)


-- instrução NOP
-- execNOP(mem,acc,eqz) = (mem,acc,eqz)
execNOP :: (ListMemoria, Int, Int) -> (ListMemoria, Int, Int) 
execNOP (mem,acc,eqz) = id -- provavelmente incompleto
