module Tarefa1Spec (testesTarefa1) where

import Test.HUnit
import Definicoes
import Tarefa1
import LI12324

-- | Colisoes entre personagens

p1 = Personagem (0,0) Jogador (50,40) Este (30,40) False False 10 0 (False, 0.0) False 0
p2 = Personagem (0,0) Fantasma (40,40) Oeste (30,40) True False 2 0 (False, 0.0) False 0

teste1 = "T1: Personagens colidem " ~: True ~=? colisoesPersonagens p1 p2

p3 = Personagem (0,0) Jogador (20,70) Este (30,40) False False 10 0 (False, 0.0) False 0
p4 = Personagem (0,0) Fantasma (60,40) Oeste (30,40) True False 2 0 (False, 0.0) False 0

teste2 = "T2: Personagens nao colidem " ~: False ~=? colisoesPersonagens p3 p4

p5 = Personagem (0,0) Jogador (40,20) Este (30,40) False False 10 0 (False, 0.0) False 0
p6 = Personagem (0,0) Fantasma (30,30) Oeste (30,40) True False 2 0 (False, 0.0) False 0

teste3 = "T3: Personagens colidem " ~: True ~=? colisoesPersonagens p5 p6

-- | Colisoes com paredes

blocos1 :: [[Char]]
blocos1 = [
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['B','B','B','A','B','B','B','B','V','V','B','B','A','B','B'],
 ['V','V','V','V','V','V','V','V','V','V','V','E','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','E','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','E','V','V','V'],
 ['B','B','B','B','B','V','B','B','V','V','B','B','B','B','B'],
 ['V','V','V','V','V','V','E','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','E','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','E','V','V','V','V','V','V','V','V'],
 ['B','B','A','B','B','B','B','B','V','V','B','B','B','B','B'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['B','B','B','B','B','B','B','B','B','B','B','B','B','B','B'],
 ['V','V','V','V','E','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','E','V','V','V','V','B','V','V','V','V','V'],
 ['V','V','V','V','E','V','B','V','V','B','V','V','V','V','V'],
 ['B','B','B','B','B','B','B','B','B','B','B','B','B','B','B']]

gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) (listaBlocos (f blocos1 (-280, 380)))

pl1 = Personagem (0.0,0.0) Jogador (8.5,6.5) Este (0,0) False False 10 0 (False, 0.0) False 0

teste4 = "T4: Jogador nao colide com nenhuma parede " ~: False ~=? colisoesParede gameMap1 pl1

pl2 = Personagem (0.0,0.0) Jogador (300,6.5) Este (1,1) False False 10 0 (False, 0.0) False 0

teste5 = "T5: Jogador colide com limite lateral " ~: 280 ~=? limiteMapaX 0 pl2 gameMap1

pl3 = Personagem (0.0,0.0) Jogador (8.5,1000) Este (1,1) False False 10 0 (False, 0.0) False 0

teste6 = "T6: Jogador colide com limite superior " ~: 380 ~=? limiteMapaY 0 pl3 gameMap1

pl4 = Personagem (0.0,0.0) Jogador (0,300) Este (1,1) False False 10 0 (False, 0.0) False 0

teste7 = "T7: Jogador colide com 'P' " ~: True ~=? colisoesChao gameMap1 pl4

testesTarefa1 = test [teste1, teste2, teste3, teste4, teste5, teste6, teste7]
