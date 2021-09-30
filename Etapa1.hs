module Main where

main = do{
  putStrLn "\n\nBomberman - Etapa 1 (Falta a implemenacao das Bombas e explosoes)\n\n";
  putStrLn "\n\nTabuleiro e Lista de Jogadores Criados\n\n"; print(primeiroMovimento);
  putStrLn "\n\nMovimentando o Jogador 2 para o leste\n\n";print(movimenta 2 'L' primeiroMovimento) ;
  putStrLn "\n\nMovimentando o Jogador 2 para o leste novamente para que assim ele pegue o presente\n\n"; print(movimenta 2 'L' (movimenta 2 'L' primeiroMovimento))
}



{-     **************** Bomberman - Etapa 1 ***************

Alunos : Tiago de Paula Alves (12011BCC040)
         Bruno Ferreira Tomé (12011BCC050)  



*********** Partes que na Teoria estao prontas, mesmo que precisem ser otimizadas:   ***********

- Criar o tabuleiro : cria as linhas e as linhas criam as celulas 

-Movimentação do Persongem  : pega um personagem (1 a 4) e pega as informações dele assim como a direção desejada verifica se ela é possivel (caso seja o tabuleiro muda se tiver um presente no local se for apenas grama nao muda) e caso seja realiza o movimento e coleta o presente e incrementa sua posição nas capacidades, e é retornado um movimento que é uma tupla que contem o atual tabuleiro e Jogadores

***********  Ainda Precisa serem implemetatadas: ***********

-- Arremesso e Explosões 

-}
-- a = Patins, b - Bomba, c = Arremesso
type Capacidades = (Int, Int, Int)
type Jogadores = (Jogador_X, Jogador_X, Jogador_X, Jogador_X)
type Jogador_X = (Int, Int, Int, Capacidades) -- (qual_jogador, coordenada x, coordenada y, capacidades) 
type Item = [Char] 
type Celula = (Int, Int, Item)
type Linha = (Celula, Celula, Celula, Celula, Celula, Celula, Celula, Celula)
type Tabuleiro = (Linha, Linha, Linha, Linha, Linha, Linha, Linha, Linha) 
type Move = (Tabuleiro, Jogadores) 


-- ********* Parte de Movimentacao **********

-- Decide se move ou nao, recebe como parametro a coordenada atual do personagem e qual a direcao desejada por char, entao tais variaveis sao encaminhadas para outra funcao que ira vefificar se existe conteudo nela ou nao, retornando um Bool

-- Cria o mapa e os jogadores
primeiroMovimento :: Move
primeiroMovimento = (gerarTabuleiro, (introJogadores (gerarTabuleiro)))

-- Movimenta o persoagem, recebe qual o jogador, a direção que deseja ir e um movimento ja existente (Tabuleiro, Jogadores)
movimenta :: Int -> Char -> Move -> Move
movimenta qJ d (tab, j) = 
  let jogador = infoJogadores qJ j 
      (x1,y1) = direcao jogador d 
      c = possivelMovimento (x1,y1) tab
      nJ = adicionarCapacidade qJ (x1,y1) c j
      nTab =  mudarTabuleiro (x1,y1) c tab

  in 
      (nTab, nJ)


direcao :: Jogador_X -> Char -> (Int,Int)
direcao (iD,x,y,cap) c  | (c == 'N') =  (x+1, y)  
                        | (c == 'S') =  (x-1, y) 
                        | (c == 'L') =  (x, y+1) 
                        | (c == 'O') =  (x, y-1) 


-- verifica se mov possivel recebe a coordenada e o tabuleiro 

possivelMovimento :: (Int,Int) -> Tabuleiro -> Char
possivelMovimento (x,y) (l0,l1,l2,l3,l4,l5,l6,l7)   | x == 0 = (verificaLinha y l0) 
                                                    | x == 1 = (verificaLinha y l1)   
                                                    | x == 2 = (verificaLinha y l2)   
                                                    | x == 3 = (verificaLinha y l3)   
                                                    | x == 4 = (verificaLinha y l4)   
                                                    | x == 5 = (verificaLinha y l5)    
                                                    | x == 6 = (verificaLinha y l6)    
                                                    | x == 7 = (verificaLinha y l7) 

verificaLinha :: Int -> Linha -> Char
verificaLinha y (c0,c1,c2,c3,c4,c5,c6,c7) | y == 0 = (verificaCelula c0) 
                                          | y == 1 = (verificaCelula c1)   
                                          | y == 2 = (verificaCelula c2)  
                                          | y == 3 = (verificaCelula c3) 
                                          | y == 4 = (verificaCelula c4)   
                                          | y == 5 = (verificaCelula c5)  
                                          | y == 6 = (verificaCelula c6)  
                                          | y == 7 = (verificaCelula c7) 

verificaCelula :: Celula -> Char
verificaCelula (_,_,[]) = error "Error. Lista vazia"
verificaCelula (_,_,[x]) = if x == 'g' then 'g'
                           else 'N'
verificaCelula (_,_,(x:y)) =  if x == 'g' && head y == 'a' then 'a'
                              else if x == 'g' && head y == 'b' then 'b'
                              else if x == 'g' && head y == 'c' then 'c'
                              else 'N'
                              




-- ************ Crição do Mapa ***************

gerarTabuleiro :: Tabuleiro  
gerarTabuleiro = (linha0,linha1,linha2,linha3,linha4,linha5,linha6,linha7)
                  where linha0 = gerarLinha 0   
                        linha1 = gerarLinha 1  
                        linha2 = gerarLinha 2  
                        linha3 = gerarLinha 3  
                        linha4 = gerarLinha 4  
                        linha5 = gerarLinha 5  
                        linha6 = gerarLinha 6  
                        linha7 = gerarLinha 7  

gerarLinha :: Int -> Linha  
gerarLinha x = (celula0, celula1, celula2, celula3, celula4, celula5, celula6, celula7)
                where celula0 = gerarItens x 0 
                      celula1 = gerarItens x 1 
                      celula2 = gerarItens x 2 
                      celula3 = gerarItens x 3 
                      celula4 = gerarItens x 4 
                      celula5 = gerarItens x 5 
                      celula6 = gerarItens x 6 
                      celula7 = gerarItens x 7 

-- grama('g') -> Capacidaes('a','b','c') 
-- grama -> jogador_X ('1', '2', '3', '4')
-- grama -> parede('p')
-- grama -> parede -> fogo ('f')
-- buraco ('b')
-- pedra('#')

gerarItens :: Int -> Int -> Celula
gerarItens x y  | x < 8 && y == 0 = (x,y,['#'])
                | x == 0 && y < 8 = (x,y,['#'])
                | x < 8 && y == 7 = (x,y,['#'])
                | x == 7 && y < 8 = (x,y,['#'])
                | x == 6 && y == 3 = (x,y,['g','a'])
                | x == 2 && y == 6 = (x,y,['g','a'])
                | x == 5 && y == 4 = (x,y,['g','b'])
                | x == 2 && y == 3 = (x,y,['g','b'])
                | x == 6 && y == 2 = (x,y,['g','b'])
                | x == 3 && y == 6 = (x,y,['g','c'])
                | x == 3 && y == 3 = (x,y,['g','c'])
                | x == 5 && y == 5 = (x,y,['g','v'])
                | x == 4 && y == 3 = (x,y,['g','v'])
                | otherwise = (x,y,['g'])

-- verifica se o mapa esta na ordem certa (nao foi necessario seu uso por enquanto)
prioridadeCelula :: Celula -> Bool
prioridadeCelula (x,y,c)  | cabeça == 'g' && (head cauda == 'a' ||  head cauda == 'b' || head cauda == 'c') = True
                          | cabeça == 'g' && (head cauda == '1' ||  head cauda == '2' || head cauda == '3' || head cauda == '4') = True
                          | cabeça == 'g' && (head cauda == 'p' &&  tail cauda == []) = True
                          | cabeça == '#' && cauda == [] = True
                          | otherwise = False
                          where cabeça = head c
                                cauda = tail c


-- ************ Crição dos Jogadores ***************

-- Introduz os jogadoes no jogo com uma movimentação diferenciada para que crie o mapa uma menor quantidade de vezes

introJogadores :: Tabuleiro -> Jogadores
introJogadores (linha0,linha1,linha2,linha3,linha4,linha5,linha6,linha7)  = (jogador1, jogador2, jogador3, jogador4)
                                      where
                                       jogador1 = localDisponivel 1 (0,0,0) linha1 
                                       jogador2 = localDisponivel 2 (0,0,0) linha3 
                                       jogador3 = localDisponivel 3 (0,0,0) linha5 
                                       jogador4 = localDisponivel 4 (0,0,0) linha6 

localDisponivel :: Int -> Capacidades -> Linha -> Jogador_X                  
localDisponivel z cap ((x0, y0, c0), (x1, y1, c1), (x2, y2, c2), (x3, y3, c3), (x4, y4, c4), (x5, y5, c5), (x6, y6, c6), (x7, y7, c7))
                                                | celulaDisponivel c0 == True = (z,x0,x0,cap)  
                                                | celulaDisponivel c1 == True = (z,x1,y1,cap) 
                                                | celulaDisponivel c2 == True = (z,x2,y2,cap) 
                                                | celulaDisponivel c3 == True = (z,x3,y3,cap) 
                                                | celulaDisponivel c4 == True = (z,x4,y4,cap) 
                                                | celulaDisponivel c5 == True = (z,x5,y5,cap) 
                                                | celulaDisponivel c6 == True = (z,x6,y6,cap) 
                                                | celulaDisponivel c7 == True = (z,x7,y7,cap) 

celulaDisponivel :: Item -> Bool
celulaDisponivel [] = error "Error. Lista vazia"
celulaDisponivel [x] = if x == 'g' then True
                           else False


-- ************ Auxiliares: ***************

infoJogadores :: Int -> Jogadores -> Jogador_X
infoJogadores j (j1,j2,j3,j4) | j == 1 = j1 
                              | j == 2 = j2   
                              | j == 3 = j3  
                              | j == 4 = j4  


-- ************ Refazer o Mapa  ***************

mudarTabuleiro :: (Int,Int) -> Char -> Tabuleiro -> Tabuleiro
mudarTabuleiro (x,y) c (l0,l1,l2,l3,l4,l5,l6,l7)
                                              | c == 'g' = (l0,l1,l2,l3,l4,l5,l6,l7)
                                              | c == 'N' = error "Nao é possivel mover"
                                              | x == 0 = (linha0,l1,l2,l3,l4,l5,l6,l7)
                                              | x == 1 = (l0,linha1,l2,l3,l4,l5,l6,l7)
                                              | x == 2 = (l0,l1,linha2,l3,l4,l5,l6,l7)
                                              | x == 3 = (l0,l1,l2,linha3,l4,l5,l6,l7)
                                              | x == 4 = (l0,l1,l2,l3,linha4,l5,l6,l7)
                                              | x == 5 = (l0,l1,l2,l3,l4,linha5,l6,l7)
                                              | x == 6 = (l0,l1,l2,l3,l4,l5,linha6,l7)
                                              | x == 7 = (l0,l1,l2,l3,l4,l5,l6,linha7)
                                              where linha0 = mudarLinha (x,y) l0
                                                    linha1 = mudarLinha (x,y) l1
                                                    linha2 = mudarLinha (x,y) l2
                                                    linha3 = mudarLinha (x,y) l3
                                                    linha4 = mudarLinha (x,y) l4
                                                    linha5 = mudarLinha (x,y) l5
                                                    linha6 = mudarLinha (x,y) l6
                                                    linha7 = mudarLinha (x,y) l7

mudarLinha :: (Int,Int) ->  Linha -> Linha
mudarLinha (x,y) (c0,c1,c2,c3,c4,c5,c6,c7)  | y == 0 = (celula0,c1,c2,c3,c4,c5,c6,c7)  
                                            | y == 1 = (c0,celula1,c2,c3,c4,c5,c6,c7) 
                                            | y == 2 = (c0,c1,celula2,c3,c4,c5,c6,c7) 
                                            | y == 3 = (c0,c1,c2,celula3,c4,c5,c6,c7)   
                                            | y == 4 = (c0,c1,c2,c3,celula4,c5,c6,c7)  
                                            | y == 5 = (c0,c1,c2,c3,c4,celula5,c6,c7)   
                                            | y == 6 = (c0,c1,c2,c3,c4,c5,celula6,c7) 
                                            | y == 7 = (c0,c1,c2,c3,c4,c5,c6,celula7)
                                            where celula0 = mudaCelula (x,y) 
                                                  celula1 = mudaCelula (x,y) 
                                                  celula2 = mudaCelula (x,y) 
                                                  celula3 = mudaCelula (x,y) 
                                                  celula4 = mudaCelula (x,y) 
                                                  celula5 = mudaCelula (x,y) 
                                                  celula6 = mudaCelula (x,y) 
                                                  celula7 = mudaCelula (x,y) 

mudaCelula :: (Int, Int) -> Celula
mudaCelula (x,y) = (x,y,['g'])


-- ************ Refazer o jogador  ***************

adicionarCapacidade :: Int -> (Int,Int) -> Char -> Jogadores -> Jogadores
adicionarCapacidade qj (x,y) c (jogador1, jogador2, jogador3, jogador4) 
                                      | c == 'a' && qj == 1 = ((qj,x,y,(1,0,0)), jogador2, jogador3, jogador4) 
                                      | c == 'c' && qj == 1 = ((qj,x,y,(0,0,1)), jogador2, jogador3, jogador4)
                                      | c == 'a' && qj == 2 = (jogador1, (qj,x,y,(1,0,0)), jogador3, jogador4)
                                      | c == 'c' && qj == 2 = (jogador1, (qj,x,y,(0,0,1)), jogador3, jogador4)
                                      | c == 'a' && qj == 3 = (jogador1, jogador2, (qj,x,y,(1,0,0)), jogador4)
                                      | c == 'c' && qj == 3 = (jogador1, jogador2, (qj,x,y,(0,0,1)), jogador4)
                                      | c == 'a' && qj == 4 = (jogador1, jogador2,jogador3 , (qj,x,y,(1,0,0)))
                                      | c == 'c' && qj == 4 = (jogador1, jogador2, jogador3, (qj,x,y,(0,0,1)))
                                      | otherwise = (moverJogador qj (x,y) (jogador1, jogador2, jogador3, jogador4))
          
moverJogador :: Int -> (Int, Int) -> Jogadores -> Jogadores
moverJogador qj (xN,yN) ((qj1, x0, y0, c0), (qj2, x1, y1, c1), (qj3, x2, y2, c2), (qj4, x3, y3, c3))
                                      | qj == 1 = ((qj1, xN, yN, c0), (qj2, x1, y1, c1), (qj3, x2, y2, c2), (qj4, x3, y3, c3))
                                      | qj == 2 = ((qj1, x0, y0, c0), (qj2, xN, yN, c1), (qj3, x2, y2, c2), (qj4, x3, y3, c3))
                                      | qj == 3 = ((qj1, x0, y0, c0), (qj2, x1, y1, c1), (qj3, xN, yN, c2), (qj4, x3, y3, c3))
                                      | qj == 4 = ((qj1, x0, y0, c0), (qj2, x1, y1, c1), (qj3, x2, y2, c2), (qj4, xN, yN, c3))
                                
