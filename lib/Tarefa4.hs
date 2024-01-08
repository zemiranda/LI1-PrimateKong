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


atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza listaAInimigos acao jogo@(Jogo { mapa = mapaA , inimigos = listaI , colecionaveis = listaCol , jogador = jogadorA }) = 
                                      let jogadorAtualiza =  atualizaJogador acao jogadorA mapaA
                                          inimigosAtualiza = atualizaInimigos listaAInimigos listaI
                                      in jogo { inimigos = inimigosAtualiza , jogador = jogadorAtualiza }


atualizaInimigos :: [Maybe Acao] -> [Personagem] -> [Personagem] 
atualizaInimigos (acaoI:t) (inimigo:t2) = (inimigo:t2)


atualizaJogador :: Maybe Acao -> Personagem -> Mapa -> Personagem 
atualizaJogador (Just AndarDireita) jogador@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir }) mapa
 = jogador { velocidade = (100,yVel) , direcao = Este} 
atualizaJogador (Just AndarEsquerda) jogador@(Personagem { posicao = (x,y), velocidade = (xVel,yVel) , direcao = dir })  mapa
 = jogador { velocidade = ((-100,yVel)) , direcao = Oeste}
atualizaJogador (Just Parar) jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) , querSaltar = quer }) mapa
    =  jogador { velocidade = (0,yVel)}  
     
atualizaJogador (Just Saltar) jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) }) mapa
    | not (colisoesChao mapa jogador) = jogador
    | otherwise = jogador { posicao = (x,y+2) , velocidade = (0,300), querSaltar = True } 
atualizaJogador Nothing jogador@(Personagem { posicao = (x,y) , velocidade = (xVel,yVel) }) mapa
    = jogador 
atualizaJogador acao jogador mapa = jogador  

