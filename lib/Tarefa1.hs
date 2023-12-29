{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Fernando Brito Ferreira <a106878@alunos.uminho.pt>
              José Diogo Carvalho Barreira Miranda Fernandes <a104159@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa ((xi,yi),d) (xf,yf) []) _ = False
colisoesParede (Mapa ((xi,yi),d) (xf,yf) (linha:t)) personagem
    | colisoesParedesLinha linha personagem = True 
    | otherwise = colisoesParede (Mapa ((xi,yi),d) (xf,yf) t) personagem

colisoesParedesLinha :: [Bloco] -> Personagem -> Bool
colisoesParedesLinha [] _ = False
colisoesParedesLinha (h:t) personagem
    | colisoesParedesAux h personagem = True 
    | otherwise = colisoesParedesLinha t personagem

colisoesParedesAux :: Bloco -> Personagem -> Bool
colisoesParedesAux (Plataforma (xs, ys)) (Personagem {posicao = (x, y)})
    | ((x + 15 == xs - 20 || x - 15 == xs + 20) && (y < ys + 20 || y > ys - 20))
     ||((y - 20 == ys + 20 || y + 20 == ys - 20) && (x < xs + 20 || x > xs - 20)) = True
    | otherwise = False
colisoesParedesAux _ (Personagem {posicao = (x, _)}) = False

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens (Personagem {posicao =(x,y), tamanho=(l,a)}) (Personagem {posicao =(x2,y2), tamanho=(l2,a2)})
                    | ((x+l) > (x2 - l2) && (x-l) < (x2 + l2)) && y - a < y2 + a2 && y + a > y2 - a2 = True
                    | otherwise = False
