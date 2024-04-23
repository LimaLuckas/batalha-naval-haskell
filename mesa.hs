import System.Random

data TipoNavio = Submarino | Destruidor | Cruzador deriving (Show, Eq)

data Navio = Navio { tipoNavio :: TipoNavio, posicoes :: Int } deriving (Show)


type Matriz = [[Char]]


criarMatriz :: Matriz
criarMatriz = replicate 10 (replicate 10 'O')


adicionarVetor :: Matriz -> Int -> Int -> [Int] -> Matriz
adicionarVetor matriz i j vetor
    | length vetor > 10 = error "O comprimento do vetor é maior que o tamanho da matriz."
    | i < 0 || i >= 10 || j < 0 || j >= 10 = error "Posição inicial fora dos limites da matriz."
    | i + length vetor > 10 = error "O vetor não cabe na matriz a partir da posição inicial."
    | otherwise = adicionarVetor' matriz i j vetor
    where
        adicionarVetor' :: Matriz -> Int -> Int -> [Int] -> Matriz
        adicionarVetor' [] _ _ _ = []
        adicionarVetor' (linha:rest) indiceLinha indiceColuna vetor
            | indiceLinha == 0 = adicionarVetorALinha linha indiceColuna vetor : adicionarVetor' rest (indiceLinha - 1) indiceColuna vetor
            | otherwise = linha : adicionarVetor' rest (indiceLinha - 1) indiceColuna vetor

        adicionarVetorALinha :: [Int] -> Int -> [Int] -> [Int]
        adicionarVetorALinha [] _ _ = []
        adicionarVetorALinha (x:xs) indiceColuna vetor
            | indiceColuna == 0 = vetor ++ adicionarVetorALinha xs (indiceColuna - 1) vetor
            | otherwise = x : adicionarVetorALinha xs (indiceColuna - 1) vetor

-- Função para gerar um vetor aleatório de acordo com o número de posições de um navio
gerarVetor :: Navio -> IO [Int]
gerarPosicao navio = do
    take (posicoes navio) . randomRs (0, 9) <$> newStdGen

-- Função para gerar uma posição aleatória na matriz
gerarPosicao :: IO (Int, Int)
gerarPosicao = do
    gen <- newStdGen
    let (i, gen') = randomR (0, 9) gen
        (j, _) = randomR (0, 9) gen'
    return (i, j)

-- Função para verificar se o jogador acertou a posição do navio ou não
verificarJogada :: (Int, Int) -> Matriz -> IO Matriz
verificarJogada(i, j) matriz = do
    let elemento = matriz !! i !! j
    if elemento /= 'O'
        then do
            putStrLn "Você acertou um navio!"
            return $ adicionarElemento matriz i j 'X'
        else do
            putStrLn "Você não acertou um navio."
            return $ adicionarElemento matriz i j '-'

-- Função para substituir um elemento em determinada posição do tabuleiro
adicionarElemento :: Matriz -> Int -> Int -> Char -> Matriz
adicionarElemento matriz i j elemento =
    let (antes, linha:depois) = splitAt i matriz
        (antes', _:depois') = splitAt j linha
    in antes ++ [antes' ++ elemento : depois'] ++ depois 

imprimirTabuleiro :: Matriz -> IO ()
imprimirTabuleiro = mapM_ putStrLn




    

