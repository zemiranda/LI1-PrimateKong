{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Fernando Brito Ferreira <a106878@alunos.uminho.pt>
              José Diogo Carvalho Barreira Miranda Fernandes <a104159@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324

import Tarefa3

import Tarefa1


atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza listaAInimigos acao jogo@(Jogo { mapa = mapaA , inimigos = listaI , colecionaveis = listaCol , jogador = jogadorA }) = 
                                      let jogadorAtualiza =  atualizaJogador acao jogadorA mapaA
                                          inimigosAtualiza = atualizaInimigos listaAInimigos listaI
                                      in jogo { inimigos = inimigosAtualiza , jogador = jogadorAtualiza }


atualizaInimigos :: [Maybe Acao] -> [Personagem] -> [Personagem]
atualizaInimigos [] [] = []
atualizaInimigos ((Just AndarDireita):t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2) 
 = (inimigo{posicao = (-280,y),velocidade = (50,yVel), direcao = Este}):atualizaInimigos t t2
atualizaInimigos ((Just AndarEsquerda):t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2) 
 = (inimigo{posicao = (280,y),velocidade = (-50,yVel), direcao = Oeste}):atualizaInimigos t t2
atualizaInimigos (Nothing:t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2) 
 = inimigo:atualizaInimigos t t2
atualizaInimigos acao inimigo = inimigo


atualizaJogador :: Maybe Acao -> Personagem -> Mapa -> Personagem 
atualizaJogador (Just AndarDireita) jogador@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }) mapa
 = jogador { velocidade = (125,yVel) , direcao = Este} 
atualizaJogador (Just AndarEsquerda) jogador@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir })  mapa
 = jogador { velocidade = ((-125,yVel)) , direcao = Oeste}
atualizaJogador (Just Parar) jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) , querSaltar = quer }) mapa
    =  jogador { velocidade = (0,yVel)}  
atualizaJogador (Just Subir) jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) , emEscada = emEsc }) mapa@(Mapa (pos,dire) posf matriz)
    | (colisoesChao mapa jogador) && (colideEscada (concat matriz) jogador) && not emEsc = jogador { posicao = (x,(y+9)), velocidade = (0,50),emEscada = True}
    | otherwise = jogador 
atualizaJogador (Just Saltar) jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) , emEscada = emEsc }) mapa@(Mapa (pos,dire) posf matriz)
    | (colisoesChao mapa jogador) = jogador { posicao = (x,y+2) , velocidade = (xVel,300), querSaltar = True } 
    | otherwise = jogador 
atualizaJogador (Just Descer) jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) , emEscada = emEsc }) mapa@(Mapa (pos,dire) posf matriz)
    | (colisoesChao mapa jogador) && (colideTopoEscada (concat matriz) jogador) = jogador { posicao = (x,(y-7)), velocidade = (0,-50),emEscada = True}
    | otherwise = jogador 
atualizaJogador Nothing jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) }) mapa
    = jogador 
atualizaJogador acao jogador mapa = jogador  

colideEscada :: [Bloco] -> Personagem -> Bool 
colideEscada [] jogador = False 
colideEscada ((Escada (xs,ys)):t) jogador@(Personagem{ posicao = (x,y) , emEscada = emEsc })
    |((x + 5) >= (xs - 20) && (x - 5) <= (xs + 20)) && ((y + 20) >= (ys - 20) && (y - 20) <= (ys + 20)) = True
    | otherwise = colideEscada t jogador 
colideEscada (bloco:t) jogador = colideEscada t jogador 


colideTopoEscada :: [Bloco] -> Personagem -> Bool 
colideTopoEscada [] jogador = False 
colideTopoEscada ((Escada (xs,ys)):t) jogador@(Personagem{ posicao = (x,y) , emEscada = emEsc }) 
 |(((x) >= (xs - 20) && (x) <= (xs + 20)) && (y >= (ys + 20) && (y <= (ys + 80)))) && not emEsc = True
 | otherwise = colideTopoEscada t jogador 
colideTopoEscada (h:t) jogador = colideTopoEscada t jogador 
