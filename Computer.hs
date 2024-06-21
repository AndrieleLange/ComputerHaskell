
-- Desta forma pode-se inserir na lista informada por parâmetro apenas os 
-- endereços que serão trabalhados 
-- durante a simulação.

type ListMemoria = [(Int, Int) ] -- lista de memorias

data CPU = CPU { acc :: Int, pc :: Int, memoria :: ListMemoria } deriving (Show)

-- Cria CPU
criaCPU :: Int -> Int -> ListMemoria -> CPU
criaCPU acc pc memoria = CPU { acc = acc, pc = pc, memoria = memoria }


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
readMem :: [(Int,Int)] -> Int -> Int
readMem (x:xs) e
    | e == fst x = snd x
    | e /= fst x = readMem xs e


-- Atualizar conteúdo na memória
atualizaMemoria :: Int -> Int -> ListMemoria -> ListMemoria
atualizaMemoria _ _ [] = salvaMemoria endereco cont ((e, c):xs)  -- Se a memória estiver vazia, salva
atualizaMemoria endereco cont ((e, c):xs)
    | endereco == e     = (e, cont) : xs  -- Atualiza o valor se o endereço for encontrado
    | otherwise         = atualizaMemoria endereco cont xs  -- Continua procurando


-- Int = endereço de memoria
data Instrucao = 
    | LOD Int  -- carrega para o acc
    | STO Int   -- carrega do acc para a variavel
    | JMP Int   -- pular para o endereço de memória pedido
    | JMZ Int   -- só pula se acc = 0
    | CPE Int   -- se o valor == acc, acc = 0, acc = 1
    | ADD Int   -- acc = valor + acc 
    | SUB Int   -- acc = valor - acc 
    | NOP       -- No operation
    | HLT       -- encerra processador
deriving (Show)

assembler :: Instrucao -> 
assembler (LOD n) = 
assembler (STO n) =
assembler (JMP n) =
assembler (JMZ n) =
assembler (CPE n) =
assembler (ADD n) =
assembler (SUB n) =
assembler (NOP) =
assembler (HLT) =



-- -- assembler instruções
-- assembler :: int -> int -> int -> Memoria
-- assembler val1 instrucao val2 

        




-- Tanto os códigos de instrução como o conteúdo dos endereços de memória 
-- poderão ser informados em 
-- decimal, desde que respeitados o tamanho máximo de bits correspondentes 
-- ao valor binário associado.



-- Executar um programa
-- recebe uma memória e devolve uma memória
executar :: [(Int, Int)] -> [(Int, Int)]
-- ler o arquivo e devolver a lista
-- ciclo busca, decodifica e executa
-- faz até encontrar o NOP
-- criar uma função de cada instrução


-- instrução NOP
-- execNOP(mem,acc,eqz) = (mem,acc,eqz)

execNOP :: ([(Int, Int)], Int, Int) -> ([(Int, Int)], Int, Int) 
execNOP (mem,acc,eqz) = id -- provavelmente incompleto

