{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Fernando Brito Ferreira <a106878@alunos.uminho.pt>
              José Diogo Carvalho Barreira Miranda Fernandes <a104159@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

limiteMapaX :: Float -> Personagem -> Mapa -> Double
limiteMapaX dt jogador@(Personagem{ posicao = (x,y) , velocidade = (xVel,yVel)}) mapa@(Mapa ((xi,yi),d) (xf,yf) (linha:t)) 
            | x < -281 = -280
            | x > 281 = 280
            | (colisoesParede mapa jogador) == (True,False) = (x-1)
            | (colisoesParede mapa jogador) == (False,True) = (x+1)
            |otherwise = (x + (realToFrac xVel) * (realToFrac dt))


limiteMapaY :: Float -> Personagem -> Mapa -> Double
limiteMapaY dt jogador@(Personagem{ posicao = (x,y) , velocidade = (xVel,yVel)}) mapa@(Mapa ((xi,yi),d) (xf,yf) matriz@(linha:t)) 
            | y < -340 = -340
            | y > 380 = 380
            | (colisoesChao mapa jogador) && not (querSaltar jogador)= y
            | not (querSaltar jogador) && (colisoesChao mapa jogador) = y
            |otherwise = (y + (realToFrac yVel) * (realToFrac dt)) 


colisoesParede :: Mapa -> Personagem -> (Bool,Bool)
colisoesParede (Mapa ((xi,yi),d) (xf,yf) []) _ = (False,False)
colisoesParede (Mapa ((xi,yi),d) (xf,yf) (linha:t)) personagem
    | (colisoesParedeLinha2 linha personagem) == (True,False) = (True,False)
    | (colisoesParedeLinha2 linha personagem) == (False,True) = (False,True) 
    | otherwise = colisoesParede (Mapa ((xi,yi),d) (xf,yf) t) personagem

colisoesParedeLinha2 :: [Bloco] -> Personagem -> (Bool,Bool)
colisoesParedeLinha2 [] _ = (False,False)
colisoesParedeLinha2 (h:t) personagem
    | (colisoesParedesAux2 h personagem) == (True,False) = (True,False) 
    | (colisoesParedesAux2 h personagem) == (False,True) = (False,True) 
    | otherwise = colisoesParedeLinha2 t personagem

colisoesParedesAux2 :: Bloco -> Personagem -> (Bool,Bool)
colisoesParedesAux2 (Plataforma (xs, ys)) (Personagem {posicao = (x, y), tamanho = (l,a)})
    | (((x + l/2) >= (xs - 20) && (x+l/2 < xs-17)) && (y < ys + 20 && y > ys - 20)) = (True,False)
    | (((x - l/2) <= (xs + 20) && (x-l/2 > xs+17) ) && (y < ys + 20 && y > ys -  20)) = (False,True)
    | otherwise = (False,False)
colisoesParedesAux2 _ (Personagem {posicao = (x, _)}) = (False,False)

colisoesChao :: Mapa -> Personagem -> Bool
colisoesChao (Mapa ((xi,yi),d) (xf,yf) []) _ = False
colisoesChao (Mapa ((xi,yi),d) (xf,yf) matriz) personagem
    | colisoesChaoLinha (concat matriz) personagem = True 
    | otherwise = False

colisoesChaoLinha :: [Bloco] -> Personagem -> Bool
colisoesChaoLinha [] _ = False
colisoesChaoLinha (h:t) personagem
    | colisoesChaoAux h personagem = True 
    | otherwise = colisoesChaoLinha t personagem

colisoesChaoAux :: Bloco -> Personagem -> Bool
colisoesChaoAux (Plataforma (xs, ys)) (Personagem {posicao = (x, y), tipo = Fantasma})
    |(round(y - 22) <= round(ys + 20) && (y-20)>(ys+15) ) && (x <= xs + 32 && x >= xs - 32) = True
    | otherwise = False
colisoesChaoAux (Plataforma (xs, ys)) (Personagem {posicao = (x, y)})
    |(round(y - 22) <= round(ys + 20) && (y-20)>(ys+15) ) && (x < xs + 30 && x > xs - 30) = True
    | otherwise = False
colisoesChaoAux (Alcapao (xs,ys) False _) (Personagem {posicao = (x,y), tipo = Fantasma})
    |((x + 5) >= (xs - 22) && (x - 5) <= (xs + 22)) && ((y-22) <= (ys+20) && (y - 20) >= ys) = True
    | otherwise = False
colisoesChaoAux (Alcapao (xs,ys) False _) (Personagem {posicao = (x,y)})
    |((x + 5) >= (xs - 22) && (x - 5) <= (xs + 22)) && ((y-20) <= (ys+20) && (y - 20) >= ys) = True
    | otherwise = False
colisoesChaoAux _ (Personagem {posicao = (x, _)}) = False

--((x + 5) >= (xs - 22) && (x - 5) <= (xs + 22)) && ((y-20) <= (ys+20) && (y - 20) >= ys) = True

colideEscada :: [Bloco] -> Personagem -> Bool 
colideEscada [] jogador = False 
colideEscada ((Escada (xs,ys)):t) jogador@(Personagem{ posicao = (x,y) , emEscada = emEsc })
    |((x + 5) >= (xs - 20) && (x - 5) <= (xs + 20)) && ((y + 20) >= (ys - 20) && (y - 20) <= (ys + 20)) = True
    | otherwise = colideEscada t jogador 
colideEscada (bloco:t) jogador = colideEscada t jogador 

colideTopoEscada :: [Bloco] -> Personagem -> Bool 
colideTopoEscada [] jogador = False 
colideTopoEscada ((Escada (xs,ys)):t) jogador@(Personagem{ posicao = (x,y) , emEscada = emEsc , tipo = Fantasma }) 
 |(((x) >= (xs - 20) && (x) <= (xs + 20)) && (y >= (ys + 65) && (y <= (ys + 85)))) && not emEsc = True
 | otherwise = colideTopoEscada t jogador 
colideTopoEscada ((Escada (xs,ys)):t) jogador@(Personagem{ posicao = (x,y) , emEscada = emEsc }) 
 |(((x) >= (xs - 20) && (x) <= (xs + 20)) && (y >= (ys + 20) && (y <= (ys + 85)))) && not emEsc = True
 | otherwise = colideTopoEscada t jogador 
colideTopoEscada (h:t) jogador = colideTopoEscada t jogador 



colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens (Personagem {posicao =(x,y), tamanho=(l,a)}) (Personagem {posicao =(x2,y2), tamanho=(l2,a2)})
                    | ((x+l/2) > (x2 - l2/2) && (x-l/2) < (x2 + l2/2)) && y - a/2 < y2 + a2/2 && y + a/2 > y2 - a2/2 = True
                    | otherwise = False

colisoesBordasInimigos :: Personagem -> Mapa -> Bool
colisoesBordasInimigos inimigo@(Personagem {posicao = (x,y) ,velocidade = (xVel,yVel), direcao = dire, ressalta = ressalta , emEscada = emEsc}) mapa@(Mapa ((xi,yi),d) (xf,yf) (linha:t))
 | not (colisoesChao mapa inimigo)  = True
 | round x < -275 = True
 | round x > 275 = True
 | (colisoesParede mapa inimigo) == (True,False) = True
 | (colisoesParede mapa inimigo) == (False,True) = True
 | otherwise = False
