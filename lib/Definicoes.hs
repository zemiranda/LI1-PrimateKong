module Definicoes where

import LI12324

sementeValor :: Semente
sementeValor = 436367344


larguraBloco :: Double
larguraBloco = 40

inimigosLimite1 :: Int
inimigosLimite1 = 5


inimigosLimite2 :: Int
inimigosLimite2 = 7
--------------------------------------------------------MAPA-------------------------------------------------------

mapa1 :: [[Char]]
mapa1 = [
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['B','B','B','A','B','B','B','B','V','V','B','B','A','B','B'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['B','B','B','B','B','V','B','B','A','B','B','B','B','B','B'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['B','B','A','B','B','B','B','B','A','B','B','B','B','B','B'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['B','B','B','B','B','B','A','B','B','B','B','B','B','B','B'],
 ['V','V','V','V','E','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','E','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','E','V','V','V','V','V','V','V','V','V','V'],
 ['B','B','B','B','B','B','B','B','B','B','B','B','B','B','B']]

mapa2 :: [[Char]]
mapa2 = [
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','V','V','V','V','V'],
 ['B','B','B','A','B','B','B','A','B','B','B','B','A','B','B'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','E','V','V','V','V'],
 ['V','V','V','V','V','V','V','V','V','V','3','V','V','V','V'],
 ['B','B','B','B','B','A','B','B','A','B','B','B','B','B','B'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['B','B','A','B','B','B','B','B','A','B','B','B','B','B','B'],
 ['V','V','V','V','V','V','V','E','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','E','V','V','V','V','V','V','V'],
 ['V','V','V','V','V','V','V','E','V','V','V','V','V','V','V'],
 ['B','B','B','B','A','B','B','B','B','B','B','B','B','B','B'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['V','E','V','V','V','V','V','V','V','V','V','V','V','E','V'],
 ['B','B','B','B','B','B','B','B','B','B','B','B','B','B','B']]

listaColecionaveisMapa1 :: [(Colecionavel,Posicao)]
listaColecionaveisMapa1 = [(Moeda,(270,20)),(Moeda,(-30,0)),(Moeda,(-250,-300)),(Martelo,(-200,-150))]

listaColecionaveisMapa2 :: [(Colecionavel,Posicao)]
listaColecionaveisMapa2 = [(Moeda,(270,20)),(Moeda,(-30,0)),(Moeda,(-250,-300)),(Martelo,(-200,-150))]


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
  | letra == 'A' = Alcapao (x, y) False 0: listaBlocosAux t
  | otherwise = listaBlocosAux t

listaBlocos :: [[(Double, Double, Char)]] -> [[Bloco]]
listaBlocos [] = []
listaBlocos (h:t) = listaBlocosAux h : listaBlocos t

mapa1Aux :: Mapa
mapa1Aux = Mapa ((0, 300), Oeste) (-250, 300) (listaBlocos (f mapa1 (-280, 380)))

mapa1Aux2 :: [[Bloco]]
mapa1Aux2 = (listaBlocos (f mapa1 (-280, 380)))

mapa2Aux :: Mapa
mapa2Aux = Mapa ((0, 300), Oeste) (-250, 300) (listaBlocos (f mapa2 (-280, 380)))

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



--63.333336636424065,-260.333333350718

listaInimigos :: [Personagem]
listaInimigos =
  [ Personagem
      { velocidade = (50, 0)
      , tipo       = Fantasma
      , posicao    = (-250, -180)
      , direcao    = Este
      , tamanho    = (30, 40)
      , emEscada   = False
      , ressalta   = True
      , vida       = 1
      , pontos     = 0
      , aplicaDano = (False, 0)
      , querSaltar = (False)
      , invincibilidade = 0
      }
  , Personagem
      { velocidade = (-50, 0)
      , tipo       = Fantasma
      , posicao    = (200, -340)
      , direcao    = Oeste
      , tamanho    = (10, 40)
      , emEscada   = False
      , ressalta   = True
      , vida       = 1
      , pontos     = 0
      , aplicaDano = (False, 0)
      , querSaltar = (False)
      , invincibilidade = 0
      }
  ]
