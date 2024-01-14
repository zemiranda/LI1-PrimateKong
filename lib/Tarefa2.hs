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
valida (Jogo mapa@(Mapa a pos matriz) inimigos colecionaveis jogador) = 
  validaPlataforma mapa &&
  larguraAlcapao 40 jogador &&
  validaEscadaLados matriz (plataformasComEscadas matriz) &&
  null (validaFimEscadas (listaDeEscadas $ concat matriz) (concat matriz)) &&
  validaColecionavel colecionaveis mapa &&
  validaVidaFant inimigos &&
  validaRessalta (jogador:inimigos) &&
  validaNumeroInimigos inimigos && 
  validaPosInimigos inimigos jogador


-----------------------------------------------------VERIFICAR PLATAFORMA-------------------------------------------------------------------
--Verifica se o mapa tem plataformas na base que impede o jogador de cair para fora 

validaPlataforma :: Mapa -> Bool
validaPlataforma (Mapa ((xi, yi), d) (xf, yf) matriz) =
  validaPlataformaAux (last matriz)
  where
    validaPlataformaAux :: [Bloco] -> Bool
    validaPlataformaAux [] = True
    validaPlataformaAux ((Plataforma (x, y)):t) = validaPlataformaAux t
    validaPlataformaAux (h:t) = False

---------------------------VALIDA RESSALTA DO JOGADOR E FANTASMAS-------------------------------------

validaRessalta :: [Personagem] -> Bool 
validaRessalta [] = True
validaRessalta ((Personagem { ressalta = True , tipo = Fantasma }):t) = validaRessalta t
validaRessalta ((Personagem { ressalta = False , tipo = Jogador }):t) = validaRessalta t
validaRessalta ((Personagem { ressalta = True , tipo = Jogador }):t) = False
validaRessalta ((Personagem { ressalta = False , tipo = Fantasma }):t) = False
validaRessalta (Personagem {}:t) = validaVidaFant t


--------------------------VALIDA POSICAO INICIAL DOS INIMIGOS COM A POSICAO DO JOGADOR-----------------------

validaPosInimigos :: [Personagem] -> Personagem -> Bool
validaPosInimigos [] _ = True
validaPosInimigos inimigos@((Personagem {posicao = (xs,ys)}):t) jogador@(Personagem {posicao = (x,y)}) 
  --(length $ filter (\Personagem {posicao =(xs,ys)} -> x /= xs && y /= ys) inimigos) == 0
 | xs == x && ys == y = False
 | otherwise = validaPosInimigos t jogador

------------------------------------------NUMERO MINIMO DE INIMIGOS--------------------------------------------

validaNumeroInimigos :: [Personagem] -> Bool
validaNumeroInimigos inimigos = length inimigos >= 2

----------------------------------------INIMIGOS FANTASMA TEM EXATAMENTE 1 VIDA--------------------------------

validaVidaFant :: [Personagem] -> Bool 
validaVidaFant [] = True
validaVidaFant ((Personagem { vida= numeroVida , tipo = Fantasma }):t) | numeroVida /= 1 = False
                                                     | otherwise = validaVidaFant t
validaVidaFant ((Personagem { vida= numeroVida }):t) = validaVidaFant t

-----------------------------Escadas nao podem comecar/terminar em alcapoes, e pelo menos uma-----------------------
----------------------------------das suas extremidades tem que ser do tipo Plataforma-------------------------------      

listaDeEscadas :: [Bloco] -> [Bloco]
listaDeEscadas [] = []
listaDeEscadas (escada@(Escada a):t) = (Escada a):listaDeEscadas t
listaDeEscadas (_:t) = listaDeEscadas t
 
validaFimEscadas :: [Bloco] -> [Bloco] -> [Bloco]
validaFimEscadas [] _ = []
validaFimEscadas (escada@(Escada (x,y)):t) matriz = ((validaFimEscadasAux (Escada (x,y)) matriz) ++ validaFimEscadas t matriz)

validaFimEscadasAux :: Bloco -> [Bloco] -> [Bloco]
validaFimEscadasAux _ [] = []
validaFimEscadasAux escada@(Escada (x,y)) ((Alcapao (xs,ys) _ _):t)
 | (x == xs && y+40 == ys) || (x == xs && y-40 == ys) = [escada]
 | otherwise = validaFimEscadasAux escada t
validaFimEscadasAux escada (_:t) = validaFimEscadasAux escada t

--Extremidade de escadas
{-
transposta :: [[a]] -> [[a]]
transposta ([]:_) = []
transposta x = map head x : transposta (map tail x)
-}

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


-----------------------------------------------------LARGURA ALCAPAO----------------------------------------------

larguraAlcapao :: Double -> Personagem -> Bool
larguraAlcapao largAlcapao (Personagem {tipo = Jogador, tamanho = (l, a)})
  | l > largAlcapao = False
  | otherwise = True
larguraAlcapao largAlcapao (Personagem {}) = False


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

