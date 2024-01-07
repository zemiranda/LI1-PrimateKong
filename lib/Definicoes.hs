module Definicoes where

import LI12324

sementeValor :: Semente
sementeValor = 436367345433

larguraBloco :: Double
larguraBloco = 40

--------------------------------------------------------MAPA-------------------------------------------------------

mapa1 :: [[Char]]
mapa1 = [
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['B','B','B','B','B','B','B','B','V','V','B','B','B','B','B'],
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
 ['B','B','B','B','B','B','B','B','V','V','B','B','B','B','B'],
 ['V','V','V','V','E','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','E','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','E','V','V','V','V','V','V','V','V','V','V'],
 ['B','B','B','B','B','B','B','B','B','B','B','B','B','B','B']]

f :: [[Char]] -> (Double,Double) -> [[(Double,Double,Char)]]
f [] _ = []
f (h:t) (x, y) = fAux h (x, y) : f t (x, y - 40)

fAux :: [Char] -> (Double, Double) -> [(Double, Double, Char)]
fAux [] _ = []
fAux (h:t) (x, y) = (x, y, h) : fAux t (x + 40, y)

listaBlocosAux :: [(Double, Double, Char)] -> [Bloco]
listaBlocosAux [] = []
listaBlocosAux ((x, y, letra):t)
  | letra == 'B' = Plataforma (x, y) : listaBlocosAux t
  | letra == 'E' = Escada (x, y) : listaBlocosAux t
  | letra == 'V' = Vazio : listaBlocosAux t
  | letra == 'A' = Alcapao (x, y) True : listaBlocosAux t
  | otherwise = listaBlocosAux t

listaBlocos :: [[(Double, Double, Char)]] -> [[Bloco]]
listaBlocos [] = []
listaBlocos (h:t) = listaBlocosAux h : listaBlocos t

mapa2 :: Mapa
mapa2 = Mapa ((0, 0), Oeste) (0, 500) (listaBlocos (f mapa1 (-280, 380)))

mapa3 :: [[Bloco]]
mapa3 = (listaBlocos (f mapa1 (-280, 380)))

listaPE :: [Bloco]
listaPE = plataformasComEscadas mapa2T

mapa2T :: [[Bloco]]
mapa2T = transposta (listaBlocos (f mapa1 (-280, 380)))

transposta :: [[a]] -> [[a]]
transposta ([]:_) = []
transposta x = map head x : transposta (map tail x)

plataformasComEscadas :: [[Bloco]] -> [Bloco]
plataformasComEscadas [] = []
plataformasComEscadas (coluna:t) = (pCEAux coluna) ++ plataformasComEscadas t

pCEAux :: [Bloco] -> [Bloco]
pCEAux [] = []
pCEAux [a] = []
pCEAux ((Plataforma (x,y)):(Escada (xs,ys)):t)= (Plataforma (x,y)) : pCEAux ((Escada (xs,ys)):t)
pCEAux (a:b:t) = pCEAux (b:t)


--------------------------------------------------------Personagens-------------------------------------------------------

jogador5 :: Personagem
jogador5 = Personagem
  { velocidade = (0, 0)
  , tipo       = Jogador
  , posicao    = (0, -310)
  , direcao    = Este
  , tamanho    = (30, 40)
  , emEscada   = False
  , ressalta   = False
  , vida       = 5
  , pontos     = 0
  , aplicaDano = (True, 90)
  }

--63.333336636424065,-260.333333350718

listaInimigos :: [Personagem]
listaInimigos =
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
      }
  ]
