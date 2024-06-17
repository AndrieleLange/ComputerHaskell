
-- Desta forma pode-se inserir na lista informada por parâmetro apenas os 
-- endereços que serão trabalhados 
-- durante a simulação.

type Memoria = (Int, Int) -- endereço de memória, conteúdo

type ListMemoria = [Memoria] -- lista de memorias

-- cria memoria
memoria :: Int -> Int -> Memoria
memoria end cont = (end, cont)
        salvaMemoria (end, cont)


-- Salvar na memoria
salvaMemoria :: Memoria -> ListMemoria
salvaMemoria memoria  =  memoria : ListMemoria

-- ler em memoria 
-- procura o endereço na memoria e retorna o conteudo 
lerMemoria :: Int -> ListMemoria -> Int
lerMemoria endereco lista 
     lista ((e,c):xs) 
        | e == endereco     = c
        | otherwise         = lerMemoria endereco xs


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




-- assembler instruções
assembler :: int -> int -> int -> Memoria
assembler val1 instrucao val2 

        


lod :: int -> int -> Memoria
lod  val1 val2 = 



-- Tanto os códigos de instrução como o conteúdo dos endereços de memória 
-- poderão ser informados em 
-- decimal, desde que respeitados o tamanho máximo de bits correspondentes 
-- ao valor binário associado.




