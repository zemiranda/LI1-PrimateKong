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
movimenta semente dt jogo@(Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) = 
    let (Mapa (posI,dirI) posf matriz) = (mapaD)
        gravidadeJogador = jogadorGravidade jogadorD mapaD
        jogadorMovimentado = movimentaJogador dt gravidadeJogador mapaD 
        mapaAtualizado = (Mapa (posI,dirI) posf (unconcat 15 (mudaAlcapao (concat matriz) jogadorD)))
    in jogo { jogador = jogadorMovimentado, mapa = mapaAtualizado}

jogadorGravidade ::Personagem -> Mapa -> Personagem 
jogadorGravidade jogador@(Personagem { posicao = (x,y) , velocidade = (xVel, yVel) , querSaltar = quer ,emEscada = emEsc }) mapa
        | emEsc && (colisoesChao mapa (jogador { posicao = (x,y)})) = jogador { emEscada = False , posicao = (x,(y+4))} 
        | emEsc = jogador { velocidade = (0,yVel)}
        | not quer && (colisoesChao mapa jogador) = jogador { velocidade = (xVel,0) , emEscada = False } 
        | otherwise = jogador { velocidade = (xVel,(yVel - (snd gravidade))) }

movimentaJogador :: Tempo -> Personagem -> Mapa -> Personagem 
movimentaJogador dt jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) }) mapa =
    jogador { posicao = ((limiteMapaX (realToFrac dt) jogador mapa),(limiteMapaY (realToFrac dt) jogador mapa)) , querSaltar = False}
    

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

recolherColecionavel :: Personagem -> [Colecionavel] -> [Colecionavel] -> (Personagem,[Colecionavel])
recolherColecionavel jogador [] listaInicial = (jogador,listaInicial)
recolherColecionavel jogador@(Personagem{ posicao = (x,y), aplicaDano = (armado, tempoArmado) , pontos = score }) ((Moeda (xs,ys)):t) listaInicial 
 |((y - 20) <= (ys + 20) || (y + 20) >= (ys - 20)) && ((x-15) < xs + 20 || (x+15) > xs - 20) = ((jogador { pontos = score + 1}),t)
 |otherwise = recolherColecionavel jogador t listaInicial
recolherColecionavel jogador@(Personagem{ posicao = (x,y), aplicaDano = (armado, tempoArmado) , pontos = score }) ((Martelo (xs,ys)):t) listaInicial
 |((y - 20) <= (ys + 20) || (y + 20) >= (ys - 20)) && ((x-15) < xs + 20 || (x+15) > xs - 20) = ((jogador { aplicaDano = (True,600) }),listaInicial)
 |otherwise = recolherColecionavel jogador { aplicaDano = (armado,tempoArmado-1) } t listaInicial


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



--git add todos os ficheiros 
--git commit -m "texto"
--git push origin 