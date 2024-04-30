import System.Random
import Data.Char (intToDigit)
import Control.Monad (replicateM, foldM)

data TipoNavio = Submarino | Destruidor | Cruzador deriving (Show, Eq)

data Navio = Navio { tipoNavio :: TipoNavio, posicoes :: Int } deriving (Show)

type Matriz = [[Char]]

-- Função para criar uma matriz inicial com água ('O')
criarMatriz :: Matriz
criarMatriz = replicate 10 (replicate 10 'O')

-- Função para adicionar um vetor em uma posição da matriz
adicionarVetor :: Matriz -> Int -> Int -> [Int] -> Matriz
adicionarVetor matriz i j vetor
    | length vetor > 10 = error "O comprimento do vetor é maior que o tamanho da matriz."
    | i < 0 || i >= 10 || j < 0 || j >= 10 = error "A posição inicial está fora dos limites da matriz."
    | otherwise = adicionarVetor' matriz i j (ajustarVetor vetor)
    where
        ajustarVetor :: [Int] -> [Int]
        ajustarVetor v = take (min (10 - j) (length v)) v  -- Ajusta o comprimento do vetor para caber na matriz

        adicionarVetor' :: Matriz -> Int -> Int -> [Int] -> Matriz
        adicionarVetor' [] _ _ _ = []
        adicionarVetor' (linha:rest) indiceLinha indiceColuna vetor
            | indiceLinha == 0 = adicionarVetorALinha linha indiceColuna vetor : adicionarVetor' rest (indiceLinha - 1) indiceColuna vetor
            | otherwise = linha : adicionarVetor' rest (indiceLinha - 1) indiceColuna vetor

        adicionarVetorALinha :: [Char] -> Int -> [Int] -> [Char]
        adicionarVetorALinha [] _ _ = []
        adicionarVetorALinha (x:xs) indiceColuna vetor
            | indiceColuna == 0 = vetorChar ++ adicionarVetorALinha xs (indiceColuna - 1) (drop (length vetorChar) vetor)
            | otherwise = x : adicionarVetorALinha xs (indiceColuna - 1) vetor
            where
                vetorChar = [intToDigit i | i <- vetor]
-- Função para gerar um vetor aleatório de acordo com o número de posições de um navio
gerarVetor :: Navio -> IO [Int]
gerarVetor navio = take (posicoes navio) <$> randomRs (0, 9) <$> newStdGen

-- Função para gerar uma posição aleatória na matriz
gerarPosicao :: IO (Int, Int)
gerarPosicao = do
    gen <- newStdGen
    let (i, gen') = randomR (0, 9) gen
        (j, _) = randomR (0, 9) gen'
    return (i, j)

-- Função para verificar se uma posição está dentro dos limites da matriz
posicaoValida :: Int -> Int -> Bool
posicaoValida i j = i >= 0 && i < 10 && j >= 0 && j < 10

-- Função para adicionar um navio na matriz
adicionarNavio :: Matriz -> Navio -> Int -> Int -> Matriz
adicionarNavio matriz navio i j = adicionarVetor matriz i j [1 | _ <- [1..posicoes navio]]

-- Função para criar uma lista de navios com quantidades específicas
criarNavios :: IO [Navio]
criarNavios = do
    submarinos <- replicateM 5 (return $ Navio Submarino 1)
    destruidores <- replicateM 3 (return $ Navio Destruidor 2)
    cruzadores <- replicateM 2 (return $ Navio Cruzador 3)
    return (submarinos ++ destruidores ++ cruzadores)

-- Função principal do jogo
main :: IO ()
main = do
    putStrLn "Bem-vindo ao jogo de Batalha Naval!"
    let maxJogadas = 30
    let tabuleiroInicial = criarMatriz
    navios <- criarNavios
    tabuleiroComNavios <- foldM (\tabuleiro navio -> colocarNavioAleatoriamente tabuleiro navio) tabuleiroInicial navios
    tabuleiroFinal <- loopJogo tabuleiroComNavios 0 maxJogadas
    if all (== '-') (concat tabuleiroFinal)
        then putStrLn "Todos os navios foram afundados! Parabéns, Você venceu!"
        else putStrLn "Você perdeu!"
    imprimirTabuleiro tabuleiroFinal

-- Função para o loop do jogo
loopJogo :: Matriz -> Int -> Int -> IO Matriz
loopJogo tabuleiro jogadas maxJogadas
    | jogadas == maxJogadas = return tabuleiro
    | otherwise = do
        imprimirTabuleiro tabuleiro
        putStrLn "Faça uma jogada (linha coluna):"
        (i, j) <- lerJogada
        novoTabuleiro <- verificarJogada (i, j) tabuleiro
        if all (== '-') (concat novoTabuleiro)
            then return novoTabuleiro
            else loopJogo novoTabuleiro (jogadas + 1) maxJogadas

-- Função para ler a jogada do jogador
lerJogada :: IO (Int, Int)
lerJogada = do
    putStrLn "Digite as coordenadas separadas por espaço (linha coluna):"
    entrada <- getLine
    let [linha, coluna] = map read (words entrada)
    if posicaoValida (linha - 1) (coluna - 1)
        then return (linha, coluna)
        else do
            putStrLn "Coordenadas inválidas. Tente novamente."
            lerJogada
            
-- Função para verificar se o jogador acertou a posição do navio ou não
verificarJogada :: (Int, Int) -> Matriz -> IO Matriz
verificarJogada (i, j) matriz = do
    let i' = i - 1  -- Ajuste das coordenadas fornecidas pelo usuário
        j' = j - 1  -- Ajuste das coordenadas fornecidas pelo usuário
    putStrLn $ "Verificando jogada: linha " ++ show i' ++ ", coluna " ++ show j'
    let elemento = matriz !! i' !! j'
    putStrLn $ "Elemento na posição: " ++ [elemento]
    if elemento /= 'O'
        then do
            putStrLn "Você acertou um navio!"
            return $ adicionarElemento matriz i' j' 'X'
        else do
            putStrLn "Você não acertou um navio."
            return $ adicionarElemento matriz i' j' '-'

-- Função para substituir um elemento em determinada posição do tabuleiro
adicionarElemento :: Matriz -> Int -> Int -> Char -> Matriz
adicionarElemento matriz i j elemento =
    let (antes, linha:depois) = splitAt i matriz
        (antes', _:depois') = splitAt j linha
    in antes ++ [antes' ++ elemento : depois'] ++ depois 

-- Função para imprimir o tabuleiro
imprimirTabuleiro :: Matriz -> IO ()
imprimirTabuleiro = mapM_ (putStrLn . map (\x -> if x == '1' then 'O' else x))

-- Função para colocar um navio aleatoriamente no tabuleiro
colocarNavioAleatoriamente :: Matriz -> Navio -> IO Matriz
colocarNavioAleatoriamente matriz navio = do
    (i, j) <- gerarPosicao
    if posicaoValida i j
        then return (adicionarNavio matriz navio i j)
        else colocarNavioAleatoriamente matriz navio
