module Main where

import Test.HUnit
import Definicoes
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4

jogadorT :: Personagem
jogadorT = Personagem
  { velocidade = (0, 0)
  , tipo       = Jogador
  , posicao    = (0, 350)
  , direcao    = Este
  , tamanho    = (30, 40)
  , emEscada   = False
  , ressalta   = False
  , vida       = 5
  , pontos     = 0
  , aplicaDano = (True, 90)
  , querSaltar = (False)
  }

listaInimigosT :: [Personagem]
listaInimigosT =
  [ Personagem
      { velocidade = (1, 1)
      , tipo       = Fantasma
      , posicao    = (70, 100)
      , direcao    = Oeste
      , tamanho    = (30, 40)
      , emEscada   = False
      , ressalta   = False
      , vida       = 70
      , pontos     = 0
      , aplicaDano = (False, 90)
      , querSaltar = (False)
      }
  , Personagem
      { velocidade = (1, 1)
      , tipo       = Fantasma
      , posicao    = (200, 100)
      , direcao    = Oeste
      , tamanho    = (30, 40)
      , emEscada   = False
      , ressalta   = False
      , vida       = 200
      , pontos     = 0
      , aplicaDano = (False, 90)
      , querSaltar = (False)
      }
  ]
alcapaoT :: Bloco
alcapaoT = Alcapao (0,0) False 0

mapaT :: Mapa
mapaT = Mapa ((0, 0), Oeste) (0, 500) (listaBlocos (f mapa1 (-280, 380)))

matrizT (Mapa (posI,dirI) posf matriz) = matriz

listaAlcapoes = [Alcapao (0,0) False 120, Alcapao (100,10) False 0]
listaEscadas = [Escada (0,0), Escada (100,100)]

testeLimites :: Test
testeLimites = test
        [ " Limite Mapa x" ~: 281 ~=? limiteMapaX 0 (jogadorT{posicao = (300,350)}) mapaT
        , " Limite Mapa y" ~: 380 ~=? limiteMapaY 0 (jogadorT{posicao = (300,1000)}) mapaT
        ]

testeAlcapoes :: Test
testeAlcapoes = test
        [ " Abrir Alcapao" ~: (Alcapao (0,0) True 120) ~=? abreAlcapao (Alcapao (0,0) False 120)
        , " Abrir Alcapao especifico" ~: [Alcapao (0,0) True 120, Alcapao (100,10) False 0] ~=? mudaAlcapao listaAlcapoes (jogadorT{posicao = (0,20)})  
        ]

testeMovimentos :: Test
testeMovimentos = test
        [" Em escada" ~: True ~=? colideEscada listaEscadas jogadorT{posicao = (0,20)}
        ," No topo da escada" ~: True ~=? colideTopoEscada listaEscadas jogadorT{posicao = (0,60)}
        ," No chao" ~: True ~=? colisoesChao mapaT jogadorT{posicao = (0,300)}
        ," Colisao Inimigo,Jogador perde Vida" ~: jogadorT{posicao = (80,100), vida = 4} ~=? inimigoAtinge jogadorT{posicao = (80,100)} listaInimigosT
        ]


main :: IO ()
main = runTestTTAndExit $ test [testeLimites,testeAlcapoes,testeMovimentos]
