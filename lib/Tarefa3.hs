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
    let gravidadeJogador = jogadorGravidade jogadorD mapaD
        jogadorMovimentado = movimentaJogador dt gravidadeJogador mapaD 
    in jogo { jogador = jogadorMovimentado }

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
 = inimigo { posicao = (2000,2000) } : desapareceInimigo inimigos
                                                                                         | otherwise = desapareceInimigo inimigos

gravidadeP :: Personagem -> Mapa -> Personagem
gravidadeP personagem@(Personagem { posicao = (x, y),  tamanho = (l,a), tipo = Jogador }) mapa 
  | colisoesChao mapa personagem = personagem 
  | otherwise = personagem { posicao = (x,y-10)}


colisoesChao :: Mapa -> Personagem -> Bool
colisoesChao (Mapa ((xi,yi),d) (xf,yf) []) _ = False
colisoesChao (Mapa ((xi,yi),d) (xf,yf) (linha:t)) personagem
    | colisoesChaoLinha linha personagem = True 
    | otherwise = colisoesChao (Mapa ((xi,yi),d) (xf,yf) t) personagem

colisoesChaoLinha :: [Bloco] -> Personagem -> Bool
colisoesChaoLinha [] _ = False
colisoesChaoLinha (h:t) personagem
    | colisoesChaoAux h personagem = True 
    | otherwise = colisoesChaoLinha t personagem

colisoesChaoAux :: Bloco -> Personagem -> Bool
colisoesChaoAux (Plataforma (xs, ys)) (Personagem {posicao = (x, y)})
    |(round(y - 22) <= round(ys + 20) && (y-20)>(ys+15) ) && (x < xs + 30 && x > xs - 30) = True
    | otherwise = False
colisoesChaoAux _ (Personagem {posicao = (x, _)}) = False

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


limiteMapaX :: Float -> Personagem -> Mapa -> Double
limiteMapaX dt jogador@(Personagem{ posicao = (x,y) , velocidade = (xVel,yVel)}) mapa@(Mapa ((xi,yi),d) (xf,yf) (linha:t)) 
            | x < -281 = -281
            | x > 281 = 281
            | (colisoesParede2 mapa jogador) == (True,False) = (x-1)
            | (colisoesParede2 mapa jogador) == (False,True) = (x+1)
            |otherwise = (x + (realToFrac xVel) * (realToFrac dt))


limiteMapaY :: Float -> Personagem -> Mapa -> Double
limiteMapaY dt jogador@(Personagem{ posicao = (x,y) , velocidade = (xVel,yVel)}) mapa@(Mapa ((xi,yi),d) (xf,yf) (linha:t)) 
            | y < -380 = -380
            | y > 380 = 380
            | not (querSaltar jogador) && (colisoesChao mapa jogador) = y
            |otherwise = (y + (realToFrac yVel) * (realToFrac dt)) 


colisoesParede2 :: Mapa -> Personagem -> (Bool,Bool)
colisoesParede2 (Mapa ((xi,yi),d) (xf,yf) []) _ = (False,False)
colisoesParede2 (Mapa ((xi,yi),d) (xf,yf) (linha:t)) personagem
    | (colisoesParedeLinha2 linha personagem) == (True,False) = (True,False)
    | (colisoesParedeLinha2 linha personagem) == (False,True) = (False,True) 
    | otherwise = colisoesParede2 (Mapa ((xi,yi),d) (xf,yf) t) personagem

colisoesParedeLinha2 :: [Bloco] -> Personagem -> (Bool,Bool)
colisoesParedeLinha2 [] _ = (False,False)
colisoesParedeLinha2 (h:t) personagem
    | (colisoesParedesAux2 h personagem) == (True,False) = (True,False) 
    | (colisoesParedesAux2 h personagem) == (False,True) = (False,True) 
    | otherwise = colisoesParedeLinha2 t personagem

colisoesParedesAux2 :: Bloco -> Personagem -> (Bool,Bool)
colisoesParedesAux2 (Plataforma (xs, ys)) (Personagem {posicao = (x, y)})
    | (((x + 15) >= (xs - 20) && (x+15 < xs-17)) && (y < ys + 20 && y > ys - 20)) = (True,False)
    | (((x - 15) <= (xs + 20) && (x-15 > xs+17) ) && (y < ys + 20 && y > ys -  20)) = (False,True)
    | otherwise = (False,False)
colisoesParedesAux2 _ (Personagem {posicao = (x, _)}) = (False,False)

{-
currentBlocks :: Personagem -> [Bloco] -> Float
currentBlocks personagem@(Personagem { velocidade = (xVel,yVel)} , ) [] = if yVel world /= 0
                         
currentBlocks world ((Blocks x y):t) = if round (yPos world) == round y+40 
                                           && xPos world -12  < x+20 && xPos world + 12 > x-20
                                           then  y+40
                                           else currentBlocks world t
-}






--git add todos os ficheiros 
--git commit -m "texto"
--git push origin 