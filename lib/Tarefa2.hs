{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Fernando Brito Ferreira <a106878@alunos.uminho.pt>
              José Diogo Carvalho Barreira Miranda Fernandes <a104159@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324

valida :: Jogo -> Bool
valida = undefined

-----------------------------------------------------VERIFICAR PLATAFORMA-------------------------------------------------------------------
validaPlataforma :: Mapa -> Bool
validaPlataforma (Mapa ((xi, yi), d) (xf, yf) matriz) =
  validaPlataformaAux (last matriz)
  where
    validaPlataformaAux :: [Bloco] -> Bool
    validaPlataformaAux [] = True
    validaPlataformaAux ((Plataforma (x, y)):t) = validaPlataformaAux t
    validaPlataformaAux (h:t) = False

-----------------------------------------------------LARGURA ALCAPAO----------------------------------------------
larguraAlcapao :: Double -> Personagem -> Bool
larguraAlcapao largAlcapao (Personagem {tipo = Jogador, tamanho = (l, a)})
  | l > largAlcapao = False
  | otherwise = True
larguraAlcapao largAlcapao (Personagem {}) = False

--------------------------------------------------------VERIFICAR MAPA-------------------------------------------
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
      
validaEscadaLados :: [[Bloco]] -> [Bloco] -> Bool
validaEscadaLados [] _ = True 
validaEscadaLados (linhas:t) listaPlatEscada | validaEscadaLadosAux linhas listaPlatEscada = 
                                                    validaEscadaLados t listaPlatEscada
                                             |otherwise = False 
     

validaEscadaLadosAux :: [Bloco] -> [Bloco] -> Bool 
validaEscadaLadosAux [] _ = True 
validaEscadaLadosAux [a] _ = True
validaEscadaLadosAux (a:b:[]) listaPlatEscada 
  | elem b listaPlatEscada =
      if isEmpty a || isAlcapao a
      then False 
      else validaEscadaLadosAux [b] listaPlatEscada
  | otherwise = validaEscadaLadosAux [b] listaPlatEscada
validaEscadaLadosAux (a:b:c:t) listaPlatEscada
  | elem b listaPlatEscada = 
      if isEmpty a && isEmpty c 
      || isEmpty a && isAlcapao c 
      || isAlcapao c && isEmpty a
      || isAlcapao a && isAlcapao c 
      then False
      else validaEscadaLadosAux (b:c:t) listaPlatEscada
  | otherwise = validaEscadaLadosAux (b:c:t) listaPlatEscada

isEmpty :: Bloco -> Bool
isEmpty Vazio = True
isEmpty _ = False

isPlataforma :: Bloco -> Bool
isPlataforma (Plataforma _) = True
isPlataforma _ = False

isAlcapao :: Bloco -> Bool
isAlcapao (Alcapao _ _ _) = True
isAlcapao _ = False


validaEscadaCimaBaixo :: [[Bloco]] -> Bool
validaEscadaCimaBaixo [] = True
validaEscadaCimaBaixo (colunas:t) |escadaPlataformaAux colunas = validaEscadaCimaBaixo t
                                  |otherwise = False 
    where 
      escadaPlataformaAux :: [Bloco] -> Bool 
      escadaPlataformaAux [] = True 
      escadaPlataformaAux [a] = True 
      escadaPlataformaAux (Vazio:(Escada (x,y)):t) = False
      escadaPlataformaAux ((Escada (x,y)):Vazio:t) = False
      escadaPlataformaAux ((Alcapao (xs,ys) _ _):(Escada (x,y)):t) = False
      escadaPlataformaAux ((Escada (x,y)):(Alcapao (xs,ys) _ _):t) = False
      escadaPlataformaAux (a:b:t) = escadaPlataformaAux (b:t)

----------------------------------------------------------VERIFICAR COLECIONAVEIS--------------------------------------------------------------------

validaColecionavel :: [(Colecionavel, Posicao)] -> Mapa -> Bool 
validaColecionavel [] mapa = True
validaColecionavel ((tipo,(x,y)):t) mapa | validaColecionavelAux (tipo,(x,y)) mapa = validaColecionavel t mapa 
                                         | otherwise = False

validaColecionavelAux :: (Colecionavel, Posicao) -> Mapa -> Bool 
validaColecionavelAux (tipo,(x,y)) (Mapa ((xi,yi),d) (xf,yf) []) = True 
validaColecionavelAux (tipo,(x,y)) (Mapa ((xi,yi),d) (xf,yf) (linha:t)) 
        | validaColecionavelAux2 (tipo,(x,y)) linha = validaColecionavelAux (tipo,(x,y)) (Mapa ((xi,yi),d) (xf,yf) t)
        | otherwise = False 

validaColecionavelAux2 ::  (Colecionavel, Posicao) -> [Bloco] -> Bool 
validaColecionavelAux2 (tipo,(x,y)) [] = True 
validaColecionavelAux2 (tipo,(x,y)) ((Plataforma (xs, ys)):t) 
        | validaColecionavelAux3 (tipo,(x,y)) (xs,ys) = validaColecionavelAux2 (tipo,(x,y)) t 
        | otherwise = False
validaColecionavelAux2 (tipo,(x,y)) ((Alcapao (xs, ys) existe _):t) 
       | validaColecionavelAux3 (tipo,(x,y)) (xs,ys) = validaColecionavelAux2 (tipo,(x,y)) t 
       | otherwise = False
validaColecionavelAux2 (tipo,(x,y)) ((Escada (xs, ys)):t) 
       | validaColecionavelAux3 (tipo,(x,y)) (xs,ys) = validaColecionavelAux2 (tipo,(x,y)) t 
       | otherwise = False
validaColecionavelAux2 (tipo,(x,y)) (Vazio:t)= validaColecionavelAux2 (tipo,(x,y)) t 
       
validaColecionavelAux3 :: (Colecionavel, Posicao) -> (Double,Double) -> Bool 
validaColecionavelAux3 (tipo,(x,y)) (xs, ys)
        | x > (xs-20) && x < (xs+20) && y < (ys+20) && y > (ys-20) = False
        |otherwise = True

-------------------------------------------------VERIFICAR INIMIGOS-----------------------------------------------------------------
{-
listaPersonagem :: [Personagem]
listaPersonagem = [(Personagem { vida= 1 , tipo = Fantasma }),(Personagem { vida= 1 , tipo = Jogador }),(Personagem { vida= 2 , tipo = Fantasma })]
-}

validaVidaFant :: [Personagem] -> Bool 
validaVidaFant [] = True
validaVidaFant ((Personagem { vida= numeroVida , tipo = Fantasma }):t) | numeroVida /= 1 = False
                                                     | otherwise = validaVidaFant t
validaVidaFant ((Personagem { vida= numeroVida }):t) = validaVidaFant t

validaRessalta :: [Personagem] -> Bool 
validaRessalta [] = True
validaRessalta ((Personagem { ressalta = True , tipo = Fantasma }):t) = validaRessalta t
validaRessalta ((Personagem { ressalta = False , tipo = Jogador }):t) = validaRessalta t
validaRessalta ((Personagem { ressalta = True , tipo = Jogador }):t) = False
validaRessalta ((Personagem { ressalta = False , tipo = Fantasma }):t) = False
validaRessalta (Personagem {}:t) = validaVidaFant t
