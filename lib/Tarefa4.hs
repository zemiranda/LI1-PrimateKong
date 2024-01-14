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
import Tarefa1
import Tarefa3




atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza listaAInimigos acao jogo@(Jogo { mapa = mapaA , inimigos = listaI , colecionaveis = listaCol , jogador = jogadorA }) = 
                                      let jogadorAtualiza =  atualizaJogador acao jogadorA mapaA
                                          inimigosAtualiza = atualizaInimigos listaAInimigos listaI
                                      in jogo { inimigos = inimigosAtualiza , jogador = jogadorAtualiza }

--Transforma acoes do inimigos e modifica as variaveis 

atualizaInimigos :: [Maybe Acao] -> [Personagem] -> [Personagem]
atualizaInimigos [] [] = []
atualizaInimigos ((Just Descer):t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2)
 = (inimigo{posicao = (x,y-10),velocidade = (0,-50), direcao = Sul, emEscada = True}):atualizaInimigos t t2
atualizaInimigos ((Just Subir):t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2)
 = (inimigo{posicao = (x,y+10),velocidade = (0,50), direcao = Norte, emEscada = True}):atualizaInimigos t t2
atualizaInimigos ((Just Parar):t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2)
 = (inimigo{posicao = (x,y),velocidade = (x,0),  emEscada = False}):atualizaInimigos t t2
atualizaInimigos ((Just AndarDireita):t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2) 
 = (inimigo{posicao = (x,y),velocidade = (50,yVel), direcao = Este}):atualizaInimigos t t2
atualizaInimigos ((Just AndarEsquerda):t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2) 
 = (inimigo{posicao = (x,y),velocidade = (-50,yVel), direcao = Oeste}):atualizaInimigos t t2
atualizaInimigos (Nothing:t) (inimigo@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }):t2) 
 = inimigo:atualizaInimigos t t2
atualizaInimigos acao inimigo = inimigo

--Transforma acoes do jogador e modifica as variaveis 

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
    | (colisoesChao mapa jogador) = jogador { posicao = (x,y+3) , velocidade = (xVel,250), querSaltar = True } 
    | otherwise = jogador 
atualizaJogador (Just Descer) jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) , emEscada = emEsc }) mapa@(Mapa (pos,dire) posf matriz)
    | (colisoesChao mapa jogador) && (colideTopoEscada (concat matriz) jogador) = jogador { posicao = (x,(y-7)), velocidade = (0,-50),emEscada = True}
    | otherwise = jogador 
atualizaJogador Nothing jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) }) mapa
    = jogador 
 



