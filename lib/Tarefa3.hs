{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Fernando Brito Ferreira <a106878@alunos.uminho.pt>
              José Diogo Carvalho Barreira Miranda Fernandes <a104159@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta sem dt jogo@(Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) = 
    let (Mapa (posI,dirI) posf matriz) = (mapaD)
        gravidadeJogador = jogadorGravidade jogadorD mapaD
        gravidadeInimigo = inimigoGravidade inimigosD mapaD
        jogadorMovimentado = movimentaJogador dt gravidadeJogador mapaD 
        jogadorColecionaveis = recolherColecionavel jogadorMovimentado colecionaveisD
        listaColecionaveisMod = tirarColecionavel (snd jogadorColecionaveis) colecionaveisD
        mapaAtualizado = (Mapa (posI,dirI) posf (unconcat 15 (mudaAlcapao (concat matriz) jogadorD)))
        inimigosMovimentados = movimentaInimigos dt sem gravidadeInimigo
    in jogo { jogador = (fst jogadorColecionaveis), colecionaveis = listaColecionaveisMod, mapa = mapaAtualizado, inimigos = inimigosMovimentados}


jogadorGravidade ::Personagem -> Mapa -> Personagem 
jogadorGravidade jogador@(Personagem { posicao = (x,y) , velocidade = (xVel, yVel) , querSaltar = quer ,emEscada = emEsc }) mapa
        | emEsc && (colisoesChao mapa (jogador { posicao = (x,y)})) = jogador { emEscada = False , posicao = (x,(y+4))} 
        | emEsc = jogador { velocidade = (0,yVel)}
        | not quer && (colisoesChao mapa jogador) = jogador { velocidade = (xVel,0) , emEscada = False } 
        | otherwise = jogador { velocidade = (xVel,(yVel - (snd gravidade))) }

movimentaJogador :: Tempo -> Personagem -> Mapa -> Personagem 
movimentaJogador dt jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) }) mapa =
    jogador { posicao = ((limiteMapaX (realToFrac dt) jogador mapa),(limiteMapaY (realToFrac dt) jogador mapa)) , querSaltar = False}

-- Movimentaçao para os Inimigos

movimentaInimigos :: Tempo -> Semente -> [Personagem] -> [Personagem]
movimentaInimigos _ _ [] = []
movimentaInimigos dt sem (ini:t) = (movimentaInimigo dt sem ini):movimentaInimigos dt sem t

movimentaInimigo :: Tempo -> Semente -> Personagem -> Personagem
movimentaInimigo dt sem inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel), querSaltar = quer }) =
    inimigo{posicao = (movimentaInimigoX dt inimigo,movimentaInimigoY dt inimigo)
    , querSaltar = saltarInimigo (sem+(round x)) && not quer}

movimentaInimigoX :: Tempo -> Personagem -> Double
movimentaInimigoX dt inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) })
 = (x + (realToFrac xVel) * (realToFrac dt))

movimentaInimigoY :: Tempo -> Personagem -> Double
movimentaInimigoY dt inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) })
 = (y + (realToFrac yVel) * (realToFrac dt))

saltarInimigo :: Semente -> Bool
saltarInimigo sem = (mod (head $ geraAleatorios sem 1) 300) == 0

inimigoGravidade :: [Personagem] -> Mapa -> [Personagem]
inimigoGravidade [] _ = []
inimigoGravidade inimigos@(ini@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel), emEscada = emEsc, querSaltar = quer }):t) mapa@(Mapa a b matriz)
 | colideEscada (concat matriz) ini =( ini {emEscada = True}):inimigoGravidade t mapa
 | otherwise = ini{emEscada=False}:inimigoGravidade t mapa


{-
subirInimigo :: [Bool] -> [Personagem] -> [Personagem]
subirInimigo _ [] = []
subirInimigo [] [] = []
subirInimigo (True:t) (ini@(Personagem{direcao = dir}):t2) = ini{direcao = Norte}:subirInimigo t t2
subirInimigo (_:t) (h:t2) = h:subirInimigo t t2

--escadasInimigo (geraAleatorio sem n)
escadasInimigo :: [Int] -> [Bool]
escadasInimigo [] = []
escadasInimigo (h:t) | odd h = True:escadasInimigo t
                     | otherwise = False:escadasInimigo t
-}
-- -------------------------

modificarVidaI :: Personagem -> [Personagem] -> [Personagem]
modificarVidaI _ [] = []
modificarVidaI jogador@(Personagem { posicao = (x, y), direcao = dir, tamanho = (l,a), tipo = Jogador, aplicaDano = (armado, _) }) inimigos@(inimigo@(Personagem { posicao = (xi, yi), vida = vidaI, tipo = Fantasma, tamanho = tamanhoI }):t)
    | armado && colideI (x, y) (l,a) dir (xi, yi) tamanhoI =
        inimigo { vida = vidaI - 1 } : modificarVidaI jogador t
    | otherwise = inimigo : modificarVidaI jogador t

colideI :: Posicao -> (Double,Double) -> Direcao -> Posicao -> (Double,Double) -> Bool 
colideI (x,y) (l,a) Este (xi,yi) (li,ai) 
     |( (xi > x+(l/2)) || (xi < (x+(l/2)+l)) ) && ( (yi < y + (a/2)) || (yi > y - (a/2))) = True
     |otherwise = False
colideI (x,y) (l,a) Oeste (xi,yi) (li,ai) 
     |( (xi > (x-(l/2)-l)) || (xi < (x+(l/2))) ) && ( (yi < y + (a/2)) || (yi > y - (a/2))) = True
     |otherwise = False
           

desapareceInimigo :: [Personagem] -> [Personagem]
desapareceInimigo [] = []
desapareceInimigo inimigos@(inimigo@(Personagem { vida = vidaI , posicao = (xi,yi) }):t) | vidaI == 0 
 = inimigo { posicao = (2000,2000) } : desapareceInimigo (tail inimigos)
                                                                                         | otherwise = inimigo:desapareceInimigo (tail inimigos)


inimigoAtinge :: Personagem -> [Personagem] -> Personagem
inimigoAtinge jogador [] = jogador
inimigoAtinge jogador@(Personagem{ vida = vidaJ }) (inimigo:t) | colisoesPersonagens jogador inimigo = jogador { vida = vidaJ -1 }
                                                               | otherwise = inimigoAtinge jogador t

{-
recolherColecionavel :: Personagem -> [Colecionavel] -> [Colecionavel] -> (Personagem,[Colecionavel])
recolherColecionavel jogador [] listaInicial = (jogador,listaInicial)
recolherColecionavel jogador@(Personagem{ posicao = (x,y), aplicaDano = (armado, tempoArmado) , pontos = score }) ((Moeda (xs,ys)):t) listaInicial 
 |((y - 20) <= (ys + 20) || (y + 20) >= (ys - 20)) && ((x-15) < xs + 20 || (x+15) > xs - 20) = ((jogador { pontos = score + 1}),t)
 |otherwise = recolherColecionavel jogador t listaInicial
recolherColecionavel jogador@(Personagem{ posicao = (x,y), aplicaDano = (armado, tempoArmado) , pontos = score }) ((Martelo (xs,ys)):t) listaInicial
 |((y - 20) <= (ys + 20) || (y + 20) >= (ys - 20)) && ((x-15) < xs + 20 || (x+15) > xs - 20) = ((jogador { aplicaDano = (True,600) }),listaInicial)
 |otherwise = recolherColecionavel jogador { aplicaDano = (armado,tempoArmado-1) } t listaInicial
-}

recolherColecionavel :: Personagem -> [(Colecionavel,Posicao)] -> (Personagem,(Colecionavel,Posicao))  
recolherColecionavel jogador [] = (jogador,(Moeda,(9999,9999)))
recolherColecionavel jogador@(Personagem { posicao = (x, y), direcao = dir, tamanho = (l,a), aplicaDano = (armado, _) , pontos = pontosP }) ((Moeda,(xs, ys)):t)
    | isInRange (x,y) (Moeda,(xs, ys)) = ((jogador { pontos = (pontosP+1) }),(Moeda,(xs, ys)))
    | otherwise =  recolherColecionavel jogador t 
recolherColecionavel jogador@(Personagem { posicao = (x, y), direcao = dir, tamanho = (l,a), aplicaDano = (armado, _) , pontos = pontosP }) ((Martelo,(xs, ys)):t)    
    | isInRange (x,y) (Martelo,(xs, ys)) = ((jogador { aplicaDano = (True,600)}),(Martelo,(xs, ys)))
    | otherwise = recolherColecionavel jogador t 
    
tirarColecionavel :: (Colecionavel,Posicao) -> [(Colecionavel,Posicao)] -> [(Colecionavel,Posicao)]
tirarColecionavel c [] = []
tirarColecionavel c (cP:t) | c == cP = t
                           | otherwise = cP : tirarColecionavel c t 

isInRange :: (Double, Double) -> (Colecionavel,Posicao) -> Bool
isInRange (x, y) (Moeda,(xs, ys)) =
  (y - 20) <= (ys + 20) && (y + 20) >= (ys - 20) && (x - 15) < xs + 20 && (x + 15) > xs - 20
isInRange (x, y) (Martelo,(xs, ys)) =
  (y - 20) <= (ys + 20) && (y + 20) >= (ys - 20) && (x - 15) < xs + 20 && (x + 15) > xs - 20



desarmar :: Personagem -> Personagem 
desarmar jogador@(Personagem { aplicaDano = (armado, tempoArmado) }) | tempoArmado == 0 = (jogador { aplicaDano = (False,tempoArmado)})
                                                                     | otherwise = jogador 


{-
currentBlocks :: Personagem -> [Bloco] -> Float
currentBlocks personagem@(Personagem { velocidade = (xVel,yVel)} , ) [] = if yVel world /= 0
                         
currentBlocks world ((Blocks x y):t) = if round (yPos world) == round y+40 
                                           && xPos world -12  < x+20 && xPos world + 12 > x-20
                                           then  y+40
                                           else currentBlocks world t
-}


-- Funçao que checa se o personagem está no alcapao
--Alcapao (Double, Double) Bool
-- Usada so para verificaçao

tempoAlcapao :: Tempo
tempoAlcapao = 60

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

--Funçao para dar unconcat á matriz
unconcat :: Int -> [Bloco] -> [[Bloco]]
unconcat _ [] = []
unconcat n xs = take n xs : unconcat n (drop n xs)



colideEscada :: [Bloco] -> Personagem -> Bool 
colideEscada [] jogador = False 
colideEscada ((Escada (xs,ys)):t) jogador@(Personagem{ posicao = (x,y) , emEscada = emEsc })
    |((x + 5) >= (xs - 20) && (x - 5) <= (xs + 20)) && ((y + 20) >= (ys - 20) && (y - 20) <= (ys + 20)) = True
    | otherwise = colideEscada t jogador 
colideEscada (bloco:t) jogador = colideEscada t jogador 

--git add todos os ficheiros 
--git commit -m "texto"
--git push origin 