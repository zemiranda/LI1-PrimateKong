module Tarefa2Spec (testesTarefa2) where

import LI12324
import Tarefa2 (valida)
import Definicoes
import Test.HUnit

mapa01 :: Mapa
mapa01 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ ['V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'P', 'P', 'P', 'P', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'E', 'V', 'V', 'E', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'E', 'V', 'V', 'E', 'V', 'V', 'V'],
      ['V', 'V', 'P', 'P', 'P', 'P', 'P', 'P', 'V', 'V'],
      ['V', 'V', 'E', 'V', 'V', 'V', 'V', 'E', 'V', 'V'],
      ['V', 'V', 'E', 'V', 'V', 'V', 'V', 'E', 'V', 'V'],
      ['V', 'P', 'P', 'P', 'A', 'P', 'P', 'P', 'P', 'V'],
      ['V', 'E', 'V', 'V', 'V', 'V', 'V', 'V', 'E', 'V'],
      ['V', 'E', 'V', 'V', 'V', 'V', 'V', 'V', 'E', 'V'],
      ['P', 'P', 'P', 'P', 'P', 'P', 'P', 'P', 'P', 'P']
    ]

inimigoModelo =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Fantasma,
      posicao = (2.5, 7.6),
      direcao = Este,
      tamanho = (1, 1),
      emEscada = False,
      ressalta = True,
      vida = 1,
      pontos = 0,
      aplicaDano = (False, 0),
      querSaltar = False
    }

jogadorParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (8.5, 6.5),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0),
      querSaltar = False
    }

jogo01 :: Jogo
jogo01 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoModelo, inimigoModelo],
      colecionaveis = [],
      jogador = jogadorParado
    }

teste01 :: Test
teste01 = "T01: Jogo que respeita todas as regras é válido" ~: True ~=? valida jogo01

mapa02 :: Mapa
mapa02 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ ['V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'P', 'P', 'P', 'P', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'E', 'V', 'V', 'E', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'E', 'V', 'V', 'E', 'V', 'V', 'V'],
      ['V', 'V', 'P', 'P', 'P', 'P', 'P', 'P', 'V', 'V'],
      ['V', 'V', 'E', 'V', 'V', 'V', 'V', 'E', 'V', 'V'],
      ['V', 'V', 'E', 'V', 'V', 'V', 'V', 'E', 'V', 'V'],
      ['V', 'P', 'P', 'P', 'A', 'P', 'P', 'P', 'P', 'V'],
      ['V', 'E', 'V', 'V', 'V', 'V', 'V', 'V', 'E', 'V'],
      ['V', 'E', 'V', 'V', 'V', 'V', 'V', 'V', 'E', 'V'],
      ['P', 'P', 'V', 'P', 'P', 'P', 'P', 'P', 'P', 'P']
    ]

teste02 :: Test
teste02 = "T02: Jogo é inválido porque não tem chão completo" ~: False ~=? valida jogo01 {mapa = mapa02}

teste03 :: Test
teste03 = TestLabel "T03" $ test [testeA, testeB]
  where
    testeA = "A: Personagens do tipo Fantasma que não ressaltam tornam o jogo inválido" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {ressalta = False}, inimigoModelo]}
    testeB = "B: Jogador não pode ressaltar" ~: False ~=? valida jogo01 {jogador = jogadorParado {ressalta = True}}

teste04 :: Test
teste04 = "T04: Se na posição inicial um inimigo colidir com a posição inicial do jogador, o jogo é inválido" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {posicao = (8.0, 6.0)}, inimigoModelo]}

teste05 :: Test
teste05 = "T05: Para o jogo ser válido precisa de ter pelo menos dois inimigos" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo]}

teste06 :: Test
teste06 = "T06: Os fantasmas começam com 1 vida" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {vida = 2}, inimigoModelo]}

mapa03 :: Mapa
mapa03 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ ['V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'A', 'P', 'P', 'P', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'E', 'V', 'V', 'E', 'V', 'V', 'V'],
      ['V', 'V', 'V', 'E', 'V', 'V', 'E', 'V', 'V', 'V'],
      ['V', 'V', 'P', 'P', 'P', 'P', 'P', 'P', 'V', 'V'],
      ['V', 'V', 'E', 'V', 'V', 'V', 'V', 'E', 'V', 'V'],
      ['V', 'V', 'E', 'V', 'V', 'V', 'V', 'E', 'V', 'V'],
      ['V', 'P', 'P', 'P', 'A', 'P', 'P', 'P', 'P', 'V'],
      ['V', 'E', 'V', 'V', 'V', 'V', 'V', 'V', 'E', 'V'],
      ['V', 'E', 'V', 'V', 'V', 'V', 'V', 'V', 'E', 'V'],
      ['P', 'P', 'V', 'P', 'P', 'P', 'P', 'P', 'P', 'P']
    ]

teste07 :: Test
teste07 = "T07: As 'E's não podem terminar em 'A'" ~: False ~=? valida jogo01 {mapa = mapa03}

testesTarefa2 :: Test
testesTarefa2 = TestLabel "Tarefa2 (valida)" $ test [teste01, teste02, teste03, teste04, teste05, teste06, teste07]