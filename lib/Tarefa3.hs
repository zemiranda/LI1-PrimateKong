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
movimenta = undefined


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
    |(round(y - 20) == round(ys + 20) || round(y + 20) == round(ys - 20)) && (x < xs + 20 || x > xs - 20) = True
    | otherwise = False
colisoesChaoAux _ (Personagem {posicao = (x, _)}) = False

inimigoAtinge :: Personagem -> [Personagem] -> Personagem
inimigoAtinge jogador [] = jogador
inimigoAtinge jogador@(Personagem{ vida = vidaJ }) (inimigo:t) | colisoesPersonagens jogador inimigo = jogador { vida = vidaJ -1 }
                                                               | otherwise = inimigoAtinge jogador t

