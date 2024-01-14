{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Fernando Brito Ferreira <a106878@alunos.uminho.pt>
              José Diogo Carvalho Barreira Miranda Fernandes <a104159@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where
import Definicoes
import LI12324
import Tarefa1

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta sem dt jogo@(Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) = 
    let (Mapa (posI,dirI) posf matriz) = (mapaD)
        gravidadeJogador = jogadorGravidade jogadorD mapaD
        gravidadeInimigo = inimigoGravidade inimigosD mapaD
        jogadorMovimentado = movimentaJogador dt gravidadeJogador mapaD 
        jogadorColecionaveis = recolherColecionavel jogadorMovimentado colecionaveisD
        jogadorArmas = modificaArma ( fst jogadorColecionaveis)
        listaColecionaveisMod = tirarColecionavel (snd jogadorColecionaveis) colecionaveisD
        mapaAtualizado = (Mapa (posI,dirI) posf (unconcat 15 (mudaAlcapao (concat matriz) jogadorD)))
        inimigosMovimentados = movimentaInimigos dt mapaD (sem+ floor (fst (posicao jogadorD) + snd (posicao jogadorD))) gravidadeInimigo
        inimigosAtingidos = tirarVidaInimigos jogadorArmas inimigosMovimentados
        inimigosMortos = desapareceInimigo inimigosAtingidos 
        jogadorAtingido = inimigoAtinge jogadorArmas inimigosMortos
        mapaFinal = acabaJogo jogadorAtingido mapaAtualizado
    in jogo { jogador = jogadorAtingido, colecionaveis = listaColecionaveisMod, mapa = mapaFinal, inimigos = inimigosMortos}

----------------------------------------------------------------------------------
-- Aplica gravidade e para o jogador na plataforma caso este colida com o topo dela

jogadorGravidade ::Personagem -> Mapa -> Personagem 
jogadorGravidade jogador@(Personagem { posicao = (x,y) , velocidade = (xVel, yVel) , querSaltar = quer ,emEscada = emEsc }) mapa
        | emEsc && (colisoesChao mapa (jogador { posicao = (x,y)})) = jogador { emEscada = False , posicao = (x,(y+4))} 
        | emEsc = jogador { velocidade = (0,yVel)}
        | not quer && (colisoesChao mapa jogador) = jogador { velocidade = (xVel,0) , emEscada = False } 
        | otherwise = jogador { velocidade = (xVel,(yVel - (snd gravidade))) }

--aplica as funcoes de calcular o x e o y do jogador

movimentaJogador :: Tempo -> Personagem -> Mapa -> Personagem 
movimentaJogador dt jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) , aplicaDano = (armado,tempo)}) mapa =
    jogador { posicao = ((limiteMapaX (realToFrac dt) jogador mapa),(limiteMapaY (realToFrac dt) jogador mapa)) , querSaltar = False }

--recolhe o colecionavel aplica o efeito no jogador e retorna a lista sem esse colecionavel

recolherColecionavel :: Personagem -> [(Colecionavel,Posicao)] -> (Personagem,(Colecionavel,Posicao))  
recolherColecionavel jogador [] = (jogador,(Moeda,(9999,9999)))
recolherColecionavel jogador@(Personagem { posicao = (x, y), direcao = dir, tamanho = (l,a), aplicaDano = (armado, _) , pontos = pontosP }) ((Moeda,(xs, ys)):t)
    | isInRange (x,y) (Moeda,(xs, ys)) = ((jogador { pontos = (pontosP+1) }),(Moeda,(xs, ys)))
    | otherwise =  recolherColecionavel jogador t 
recolherColecionavel jogador@(Personagem { posicao = (x, y), direcao = dir, tamanho = (l,a), aplicaDano = (armado, _) , pontos = pontosP }) ((Martelo,(xs, ys)):t)    
    | isInRange (x,y) (Martelo,(xs, ys)) = ((jogador { aplicaDano = (True,600)}),(Martelo,(xs, ys)))
    | otherwise = recolherColecionavel jogador t 

--usando o colecionavel apanhado e a lista de colecionaveis retira o da lista 
tirarColecionavel :: (Colecionavel,Posicao) -> [(Colecionavel,Posicao)] -> [(Colecionavel,Posicao)]
tirarColecionavel c [] = []
tirarColecionavel c (cP:t) | c == cP = t
                           | otherwise = cP : tirarColecionavel c t 

isInRange :: (Double, Double) -> (Colecionavel,Posicao) -> Bool
isInRange (x, y) (Moeda,(xs, ys)) =
  (y - 20) <= (ys + 20) && (y + 20) >= (ys - 20) && (x - 15) < xs + 20 && (x + 15) > xs - 20
isInRange (x, y) (Martelo,(xs, ys)) =
  (y - 20) <= (ys + 20) && (y + 20) >= (ys - 20) && (x - 15) < xs + 20 && (x + 15) > xs - 20


--desarma o jogador se o tempo tiver acabado e descrementa o timer do martelo

modificaArma :: Personagem -> Personagem 
modificaArma jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) , aplicaDano = (True,0)}) 
 = desarmar jogador 
modificaArma jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) , aplicaDano = (True,tempo)}) 
 = jogador { aplicaDano = (True,(tempo -1))}
modificaArma jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) , aplicaDano = (armado,tempo)}) 
 = jogador

desarmar :: Personagem -> Personagem 
desarmar jogador@(Personagem { aplicaDano = (armado, tempoArmado) }) | tempoArmado == 0 = (jogador { aplicaDano = (False,tempoArmado)})
                                                                     | otherwise = jogador 

-------------------------------------------------------------------------------------------------
-- verifica se o jogador foi atingido pelo inimigo 
--,retiralhe uma vida e aplica um timer de invencibilidade para que nao perca uma vida por frame

inimigoAtinge :: Personagem -> [Personagem] -> Personagem
inimigoAtinge jogador [] = jogador
inimigoAtinge jogador@(Personagem{ vida = vidaJ , invincibilidade = 0}) (macaco@(Personagem { tipo = MacacoMalvado }):t) | colisoesPersonagens jogador macaco = jogador { vida = 0 }
                                                                                                                  | otherwise = inimigoAtinge jogador t
inimigoAtinge jogador@(Personagem{ vida = vidaJ , invincibilidade = 0}) (inimigo:t) | colisoesPersonagens jogador inimigo = jogador { vida = vidaJ -1 , invincibilidade = 1 }
                                                                                    | otherwise = inimigoAtinge jogador t
inimigoAtinge jogador@(Personagem{ vida = vidaJ , invincibilidade = tempoI}) (inimigo:t) | tempoI < 60 = jogador { invincibilidade = (tempoI+1) }
                                                                                         | otherwise = inimigoAtinge (jogador { invincibilidade = 0 }) t
--------------------------------------------------------------------------
-- verifica se a posicao do jogador e igual a posicao final do data Mapa

acabaJogo :: Personagem -> Mapa -> Mapa
acabaJogo (Personagem{posicao = (x,y), tamanho= (l,a) , pontos = pontosJ }) mapa@(Mapa i (fx, fy) m) 
 | ((((x+l/2) > fx && (x-l/2) < fx) || ((x+l/2) < fx && (x-l/2) > fx)) && (fy < y+a/2 && fy > y-a/2)) 
 = (Mapa i (posicaoFinal) m)
 | otherwise = mapa

-----------------------------------------------------------------------------
-- Para a Velocidade y do inimigo na plataforma caso este colida com o topo dela 
-- e modifica a componente emEscada caso colida com uma escada

inimigoGravidade :: [Personagem] -> Mapa -> [Personagem]
inimigoGravidade [] _ = []
inimigoGravidade inimigos@(ini@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel), emEscada = emEsc, querSaltar = quer }):t) mapa@(Mapa a b matriz)
 -- colideTopoEscada (concat matriz) ini = (ini {emEscada = True }) : inimigoGravidade t mapa 
 | colisoesChao mapa  ini && yVel /= 0 = (ini { posicao = (x,y), velocidade = (-50,0) , emEscada = False , direcao = Oeste }) : inimigoGravidade t mapa
 | colideEscada (concat matriz) ini =( ini {emEscada = True}):inimigoGravidade t mapa
 | otherwise = ini{emEscada=False}:inimigoGravidade t mapa

--aplica as funcoes que calculam a posicao x e y do inimigo e a funcao saltarInimigo que torna aleatorio a decisao do inimigo subir ou descer a escada

movimentaInimigos :: Tempo -> Mapa -> Semente -> [Personagem] -> [Personagem]
movimentaInimigos _ _ _ [] = []
movimentaInimigos dt mapa sem (ini:t) = (movimentaInimigo dt mapa sem ini):movimentaInimigos dt mapa sem t

movimentaInimigo :: Tempo -> Mapa -> Semente -> Personagem -> Personagem
movimentaInimigo dt mapa@(Mapa i (fx,fy) m) sem inimigo@(Personagem { posicao = (x, y), tipo = MacacoMalvado, velocidade = (xVel,yVel), querSaltar = quer , emEscada = emEsc }) =
    inimigo{posicao = (movimentaInimigoX dt inimigo,movimentaInimigoY dt inimigo)}
movimentaInimigo dt mapa@(Mapa i (fx,fy) m) sem inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel), querSaltar = quer , emEscada = emEsc }) =
    inimigo{posicao = (movimentaInimigoX dt inimigo,movimentaInimigoY dt inimigo)
    , querSaltar = saltarInimigo (sem*(abs(round x))) && not quer && colisoesChao mapa inimigo && (emEsc || colideTopoEscada (concat m) inimigo)  }

movimentaInimigoX :: Tempo -> Personagem -> Double
movimentaInimigoX dt inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) })
 = (x + (realToFrac xVel) * (realToFrac dt))

movimentaInimigoY :: Tempo -> Personagem -> Double
movimentaInimigoY dt inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) })
 = (y + (realToFrac yVel) * (realToFrac dt))

saltarInimigo :: Semente -> Bool
saltarInimigo sem =mod ((div (head $ geraAleatorios sem 1) 10)) 60 == 0

--Verifica se o inimigo colide com o hitbox do martelo e retira-lhe uma vida 

tirarVidaInimigos :: Personagem -> [Personagem] -> [Personagem] 
tirarVidaInimigos jogador [] = []
tirarVidaInimigos jogador@(Personagem { posicao = (xi, yi), direcao = dir, tamanho = (li,ai) , aplicaDano = (armado,tempo)}) inimigos@(inimigo@(Personagem { posicao = (x, y), direcao = dirI, tamanho = (l,a) , vida = vidaI}):t)  
 | armado && colideI jogador inimigo = (inimigo { vida = (vidaI-1) }) : tirarVidaInimigos jogador t
 | otherwise = inimigo : tirarVidaInimigos jogador t

colideI :: Personagem -> Personagem -> Bool 
colideI jogador@(Personagem { posicao = (x, y), direcao = Este, tamanho = (l,a)}) inimigo@(Personagem { posicao = (xi, yi), direcao = dirI, tipo = Fantasma,  tamanho = (li,ai)})
     |( (xi > x+(l/2)) && (xi < (x+(l/2)+l)) ) && ( (yi < y + (a/2)) && (yi > y - (a/2))) = True
     |otherwise = False
colideI jogador@(Personagem { posicao = (x, y), direcao = Oeste, tamanho = (l,a)}) inimigo@(Personagem { posicao = (xi, yi),tipo = Fantasma, direcao = dirI, tamanho = (li,ai)})
     |( (xi > (x-(l/2)-l)) && (xi < (x+(l/2))) ) && ( (yi < y + (a/2)) && (yi > y - (a/2))) = True
     |otherwise = False
colideI jogador inimigo = False         

-- se o inimigo tiver 0 vidas retira o do mapa 

desapareceInimigo :: [Personagem] -> [Personagem]
desapareceInimigo [] = []
desapareceInimigo inimigos@(inimigo@(Personagem { vida = vidaI , posicao = (xi,yi) }):t) | vidaI == 0 
 = inimigo { posicao = (2000,2000) } : desapareceInimigo (tail inimigos)
                                                                                         | otherwise = inimigo:desapareceInimigo (tail inimigos)
--------------------------------------------------------------------------------------

tempoAlcapao :: Tempo
tempoAlcapao = 60

--se o jogador colidir no topo do alcapao este passado 1 segundo vai abrir

mudaAlcapao :: [Bloco] -> Personagem -> [Bloco]
mudaAlcapao [] jogador = []
mudaAlcapao (h@(Alcapao (xs,ys) False tempo):t) jogador@(Personagem{ posicao = (x,y)})
 | tempo >= 1 && tempo < tempoAlcapao = (Alcapao (xs,ys) False (tempo+1)):mudaAlcapao t jogador
 | tempo >= tempoAlcapao = abreAlcapao h:mudaAlcapao t jogador
 |((x + 5) >= (xs - 20) && (x - 5) <= (xs + 20)) && ((y-20) <= (ys+20) && (y - 20) >= ys) = (Alcapao (xs,ys) False (1)):mudaAlcapao t jogador
 | otherwise = h:mudaAlcapao t jogador
mudaAlcapao (h:t) jogador = h:mudaAlcapao t jogador

abreAlcapao :: Bloco -> Bloco
abreAlcapao (Alcapao pos False tempoAlcapao) = (Alcapao pos True tempoAlcapao)

---------------------------------------------------------------------------

--Funçao para dar unconcat á matriz
unconcat :: Int -> [Bloco] -> [[Bloco]]
unconcat _ [] = []
unconcat n xs = take n xs : unconcat n (drop n xs)

