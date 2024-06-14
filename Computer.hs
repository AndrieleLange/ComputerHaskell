--  A memória poderá ser representada em Haskell por uma lista de tuplas. 
-- Cada tupla é formada por dois inteiros, 
-- onde o primeiro representa o endereço de memória e o segundo o conteúdo 
-- daquela posição de memória. 
-- Desta forma pode-se inserir na lista informada por parâmetro apenas os 
-- endereços que serão trabalhados 
-- durante a simulação.

type Memoria = (Int, Int) -- endereço de memória, conteúdo

type ListMemoria = [Memoria] -- lista de memorias

-- Salvar na memoria
salvaMemoria :: Int -> Int -> ListMemoria -> ListMemoria
salvaMemoria end cont memoria = (end, cont) 
    | memoria : ListMemoria

-- ler em memoria 
lerMemoria :: Int -> ListMemoria -> Int
lerMemoria endereco memoria = map ()

data Instrucao = 
    NOP
    | LOD Int
    | STO Int
    | JMP Int
    | JMZ Int
    | CPE Int
    | ADD Int
    | SUB Int
    | HLT 

-- assembler instruções
assembler :: int -> int -> int -> Memoria
assembler val1 instrucao val2 
        | 


lod :: int -> int -> Memoria
lod  val1 val2 = 



-- Tanto os códigos de instrução como o conteúdo dos endereços de memória 
-- poderão ser informados em 
-- decimal, desde que respeitados o tamanho máximo de bits correspondentes 
-- ao valor binário associado.
