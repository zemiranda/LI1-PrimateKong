{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Fernando Brito Ferreira <a106878@alunos.uminho.pt>
              José Diogo Carvalho Barreira Miranda Fernandes <a104159@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta sem dt jogo@(Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) = 
    let (Mapa (posI,dirI) posf matriz) = (mapaD)
        gravidadeJogador = jogadorGravidade jogadorD mapaD
        gravidadeInimigo = inimigoGravidade inimigosD mapaD
        jogadorMovimentado = movimentaJogador dt gravidadeJogador mapaD 
        jogadorColecionaveis = recolherColecionavel jogadorMovimentado colecionaveisD
        jogadorArmas = modificaArma ( fst jogadorColecionaveis)
        listaColecionaveisMod = tirarColecionavel (snd jogadorColecionaveis) colecionaveisD
        mapaAtualizado = (Mapa (posI,dirI) posf (unconcat 15 (mudaAlcapao (concat matriz) jogadorD)))
        inimigosMovimentados = movimentaInimigos dt sem gravidadeInimigo
        inimigosAtingidos = tirarVidaInimigos jogadorArmas inimigosMovimentados
        inimigosMortos = desapareceInimigo inimigosAtingidos 
        jogadorAtingido = inimigoAtinge jogadorArmas inimigosMortos
    in jogo { jogador = jogadorAtingido, colecionaveis = listaColecionaveisMod, mapa = mapaAtualizado, inimigos = inimigosMortos}

modificaArma :: Personagem -> Personagem 
modificaArma jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) , aplicaDano = (True,0)}) 
 = desarmar jogador 
modificaArma jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) , aplicaDano = (True,tempo)}) 
 = jogador { aplicaDano = (True,(tempo -1))}
modificaArma jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) , aplicaDano = (armado,tempo)}) 
 = jogador

jogadorGravidade ::Personagem -> Mapa -> Personagem 
jogadorGravidade jogador@(Personagem { posicao = (x,y) , velocidade = (xVel, yVel) , querSaltar = quer ,emEscada = emEsc }) mapa
        | emEsc && (colisoesChao mapa (jogador { posicao = (x,y)})) = jogador { emEscada = False , posicao = (x,(y+4))} 
        | emEsc = jogador { velocidade = (0,yVel)}
        | not quer && (colisoesChao mapa jogador) = jogador { velocidade = (xVel,0) , emEscada = False } 
        | otherwise = jogador { velocidade = (xVel,(yVel - (snd gravidade))) }

movimentaJogador :: Tempo -> Personagem -> Mapa -> Personagem 
movimentaJogador dt jogador@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) , aplicaDano = (armado,tempo)}) mapa =
    jogador { posicao = ((limiteMapaX (realToFrac dt) jogador mapa),(limiteMapaY (realToFrac dt) jogador mapa)) , querSaltar = False }


-- Movimentaçao para os Inimigos

movimentaInimigos :: Tempo -> Semente -> [Personagem] -> [Personagem]
movimentaInimigos _ _ [] = []
movimentaInimigos dt sem (ini:t) = (movimentaInimigo dt sem ini):movimentaInimigos dt sem t

movimentaInimigo :: Tempo -> Semente -> Personagem -> Personagem
movimentaInimigo dt sem inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel), querSaltar = quer , emEscada = emEsc }) =
    inimigo{posicao = (movimentaInimigoX dt inimigo,movimentaInimigoY dt inimigo)
    , querSaltar = saltarInimigo (sem*(abs(round x))) && not quer && emEsc}

movimentaInimigoX :: Tempo -> Personagem -> Double
movimentaInimigoX dt inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) })
 = (x + (realToFrac xVel) * (realToFrac dt))

movimentaInimigoY :: Tempo -> Personagem -> Double
movimentaInimigoY dt inimigo@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel) })
 = (y + (realToFrac yVel) * (realToFrac dt))

saltarInimigo :: Semente -> Bool
saltarInimigo sem = (mod (head $ geraAleatorios sem 1) 3) == 0

inimigoGravidade :: [Personagem] -> Mapa -> [Personagem]
inimigoGravidade [] _ = []
inimigoGravidade inimigos@(ini@(Personagem { posicao = (x, y), direcao = dir, velocidade = (xVel,yVel), emEscada = emEsc, querSaltar = quer }):t) mapa@(Mapa a b matriz)
 | colideEscada (concat matriz) ini =( ini {emEscada = True}):inimigoGravidade t mapa
 | colisoesChao mapa  ini = (ini { posicao = (x,y+5), velocidade = (-50,0) , emEscada = False , direcao = Oeste }) : inimigoGravidade t mapa
 | otherwise = ini{emEscada=False}:inimigoGravidade t mapa


{-
subirInimigo :: [Bool] -> [Personagem] -> [Personagem]
subirInimigo _ [] = []
subirInimigo [] [] = []
subirInimigo (True:t) (ini@(Personagem{direcao = dir}):t2) = ini{direcao = Norte}:subirInimigo t t2
subirInimigo (_:t) (h:t2) = h:subirInimigo t t2

--escadasInimigo (geraAleatorio sem n)
escadasInimigo :: [Int] -> [Bool]
escadasInimigo [] = []
escadasInimigo (h:t) | odd h = True:escadasInimigo t
                     | otherwise = False:escadasInimigo t
-}
-- -------------------------
{-
modificarVidaI :: Personagem -> [Personagem] -> [Personagem]
modificarVidaI _ [] = []
modificarVidaI jogador@(Personagem { posicao = (x, y), direcao = dir, tamanho = (l,a), tipo = Jogador, aplicaDano = (armado, _) }) inimigos@(inimigo@(Personagem { posicao = (xi, yi), vida = vidaI, tipo = Fantasma, tamanho = tamanhoI }):t)
    | armado && colideI (x, y) (l,a) dir (xi, yi) tamanhoI =
        inimigo { vida = vidaI - 1 } : modificarVidaI jogador t
    | otherwise = inimigo : modificarVidaI jogador t
-}
--(Jogo mapa inimigos@(inimigo@(Personagem { posicao = (x, y), direcao = Este, tamanho = (l,a)}):t) colecionaveis jogador@(Personagem { posicao = (xi, yi), direcao = Este, tamanho = (li,ai)}))

tirarVidaInimigos :: Personagem -> [Personagem] -> [Personagem] 
tirarVidaInimigos jogador [] = []
tirarVidaInimigos jogador@(Personagem { posicao = (xi, yi), direcao = dir, tamanho = (li,ai) , aplicaDano = (armado,tempo)}) inimigos@(inimigo@(Personagem { posicao = (x, y), direcao = dirI, tamanho = (l,a) , vida = vidaI}):t)  
 | armado && colideI jogador inimigo = (inimigo { vida = (vidaI-1) }) : tirarVidaInimigos jogador t
 | otherwise = inimigo : tirarVidaInimigos jogador t

colideI :: Personagem -> Personagem -> Bool 
colideI jogador@(Personagem { posicao = (x, y), direcao = Este, tamanho = (l,a)}) inimigo@(Personagem { posicao = (xi, yi), direcao = dirI, tamanho = (li,ai)})
     |( (xi > x+(l/2)) && (xi < (x+(l/2)+l)) ) && ( (yi < y + (a/2)) && (yi > y - (a/2))) = True
     |otherwise = False
colideI jogador@(Personagem { posicao = (x, y), direcao = Oeste, tamanho = (l,a)}) inimigo@(Personagem { posicao = (xi, yi), direcao = dirI, tamanho = (li,ai)})
     |( (xi > (x-(l/2)-l)) && (xi < (x+(l/2))) ) && ( (yi < y + (a/2)) && (yi > y - (a/2))) = True
     |otherwise = False
colideI jogador inimigo = False         

desapareceInimigo :: [Personagem] -> [Personagem]
desapareceInimigo [] = []
desapareceInimigo inimigos@(inimigo@(Personagem { vida = vidaI , posicao = (xi,yi) }):t) | vidaI == 0 
 = inimigo { posicao = (2000,2000) } : desapareceInimigo (tail inimigos)
                                                                                         | otherwise = inimigo:desapareceInimigo (tail inimigos)


inimigoAtinge :: Personagem -> [Personagem] -> Personagem
inimigoAtinge jogador [] = jogador
inimigoAtinge jogador@(Personagem{ vida = vidaJ , invincibilidade = 0}) (inimigo:t) | colisoesPersonagens jogador inimigo = jogador { vida = vidaJ -1 , invincibilidade = 1 }
                                                                                    | otherwise = inimigoAtinge jogador t
inimigoAtinge jogador@(Personagem{ vida = vidaJ , invincibilidade = tempoI}) (inimigo:t) | colisoesPersonagens jogador inimigo = jogador { invincibilidade = (tempoI+1) }
                                                                                         | otherwise = inimigoAtinge (jogador { invincibilidade = 0 }) t


{-
recolherColecionavel :: Personagem -> [Colecionavel] -> [Colecionavel] -> (Personagem,[Colecionavel])
recolherColecionavel jogador [] listaInicial = (jogador,listaInicial)
recolherColecionavel jogador@(Personagem{ posicao = (x,y), aplicaDano = (armado, tempoArmado) , pontos = score }) ((Moeda (xs,ys)):t) listaInicial 
 |((y - 20) <= (ys + 20) || (y + 20) >= (ys - 20)) && ((x-15) < xs + 20 || (x+15) > xs - 20) = ((jogador { pontos = score + 1}),t)
 |otherwise = recolherColecionavel jogador t listaInicial
recolherColecionavel jogador@(Personagem{ posicao = (x,y), aplicaDano = (armado, tempoArmado) , pontos = score }) ((Martelo (xs,ys)):t) listaInicial
 |((y - 20) <= (ys + 20) || (y + 20) >= (ys - 20)) && ((x-15) < xs + 20 || (x+15) > xs - 20) = ((jogador { aplicaDano = (True,600) }),listaInicial)
 |otherwise = recolherColecionavel jogador { aplicaDano = (armado,tempoArmado-1) } t listaInicial
-}

recolherColecionavel :: Personagem -> [(Colecionavel,Posicao)] -> (Personagem,(Colecionavel,Posicao))  
recolherColecionavel jogador [] = (jogador,(Moeda,(9999,9999)))
recolherColecionavel jogador@(Personagem { posicao = (x, y), direcao = dir, tamanho = (l,a), aplicaDano = (armado, _) , pontos = pontosP }) ((Moeda,(xs, ys)):t)
    | isInRange (x,y) (Moeda,(xs, ys)) = ((jogador { pontos = (pontosP+1) }),(Moeda,(xs, ys)))
    | otherwise =  recolherColecionavel jogador t 
recolherColecionavel jogador@(Personagem { posicao = (x, y), direcao = dir, tamanho = (l,a), aplicaDano = (armado, _) , pontos = pontosP }) ((Martelo,(xs, ys)):t)    
    | isInRange (x,y) (Martelo,(xs, ys)) = ((jogador { aplicaDano = (True,600)}),(Martelo,(xs, ys)))
    | otherwise = recolherColecionavel jogador t 
    
tirarColecionavel :: (Colecionavel,Posicao) -> [(Colecionavel,Posicao)] -> [(Colecionavel,Posicao)]
tirarColecionavel c [] = []
tirarColecionavel c (cP:t) | c == cP = t
                           | otherwise = cP : tirarColecionavel c t 

isInRange :: (Double, Double) -> (Colecionavel,Posicao) -> Bool
isInRange (x, y) (Moeda,(xs, ys)) =
  (y - 20) <= (ys + 20) && (y + 20) >= (ys - 20) && (x - 15) < xs + 20 && (x + 15) > xs - 20
isInRange (x, y) (Martelo,(xs, ys)) =
  (y - 20) <= (ys + 20) && (y + 20) >= (ys - 20) && (x - 15) < xs + 20 && (x + 15) > xs - 20



desarmar :: Personagem -> Personagem 
desarmar jogador@(Personagem { aplicaDano = (armado, tempoArmado) }) | tempoArmado == 0 = (jogador { aplicaDano = (False,tempoArmado)})
                                                                     | otherwise = jogador 


{-
currentBlocks :: Personagem -> [Bloco] -> Float
currentBlocks personagem@(Personagem { velocidade = (xVel,yVel)} , ) [] = if yVel world /= 0
                         
currentBlocks world ((Blocks x y):t) = if round (yPos world) == round y+40 
                                           && xPos world -12  < x+20 && xPos world + 12 > x-20
                                           then  y+40
                                           else currentBlocks world t
-}


-- Funçao que checa se o personagem está no alcapao
--Alcapao (Double, Double) Bool
-- Usada so para verificaçao

tempoAlcapao :: Tempo
tempoAlcapao = 60

mudaAlcapao :: [Bloco] -> Personagem -> [Bloco]
mudaAlcapao [] jogador = []
mudaAlcapao (h@(Alcapao (xs,ys) False tempo):t) jogador@(Personagem{ posicao = (x,y)})
 | tempo >= 1 && tempo < tempoAlcapao = (Alcapao (xs,ys) False (tempo+1)):mudaAlcapao t jogador
 | tempo >= tempoAlcapao = abreAlcapao h:mudaAlcapao t jogador
 |((x + 5) >= (xs - 20) && (x - 5) <= (xs + 20)) && ((y-20) <= (ys+20) && (y - 20) >= ys) = (Alcapao (xs,ys) False (1)):mudaAlcapao t jogador
 | otherwise = h:mudaAlcapao t jogador
mudaAlcapao (h:t) jogador = h:mudaAlcapao t jogador

abreAlcapao :: Bloco -> Bloco
abreAlcapao (Alcapao pos False tempoAlcapao) = (Alcapao pos True tempoAlcapao)

--Funçao para dar unconcat á matriz
unconcat :: Int -> [Bloco] -> [[Bloco]]
unconcat _ [] = []
unconcat n xs = take n xs : unconcat n (drop n xs)



colideEscada :: [Bloco] -> Personagem -> Bool 
colideEscada [] jogador = False 
colideEscada ((Escada (xs,ys)):t) jogador@(Personagem{ posicao = (x,y) , emEscada = emEsc })
    |((x + 5) >= (xs - 20) && (x - 5) <= (xs + 20)) && ((y + 20) >= (ys - 20) && (y - 20) <= (ys + 20)) = True
    | otherwise = colideEscada t jogador 
colideEscada (bloco:t) jogador = colideEscada t jogador 

--git add todos os ficheiros 
--git commit -m "texto"
--git push origin 