module Reage where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game

import Data.Maybe
import System.Random
import Definicoes
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import LI12324 

-----------------------------------------------------------------------------
-- Recebe o input do jogador no Jogo/Menu e transforma numa acao , e transmite as acoes dos inimigos

reage :: Event  -> PrimateKong -> IO PrimateKong
reage (EventKey (SpecialKey KeyRight) Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA  }) 
  | menuA == EmJogo = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just AndarDireita) jogoA }
  |otherwise = return  primata

reage (EventKey (SpecialKey KeyLeft) Down _ _)primata@(PrimateKong { jogo = jogoA , menu = menuA })
  | menuA == EmJogo = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just AndarEsquerda) jogoA }
  |otherwise = return primata 

reage (EventKey (SpecialKey KeyRight) Up _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA }) 
  | menuA == EmJogo = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Parar) jogoA }
  |otherwise = return primata 
 
reage (EventKey (SpecialKey KeyLeft) Up _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA  }) 
  | menuA == EmJogo = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Parar) jogoA }
  |otherwise = return primata   

reage (EventKey (SpecialKey KeyUp) Down _ _) primata@(PrimateKong {jogo = jogoA , menu = menuA }) 
  | menuA == EmJogo = let (Mapa (posI,dirI) posf matriz) = (mapa jogoA)
                          (Personagem { aplicaDano = (armado,tempo)}) = (jogador jogoA)
                      in if colideEscada (concat matriz) (jogador jogoA) && not armado
                         then return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Subir) jogoA }
                         else return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Saltar) jogoA }
  |otherwise = return primata 
 
reage (EventKey (SpecialKey KeyDown) Down _ _) primata@(PrimateKong { jogo = jogoA ,tema = temaA, menu = menuA })
  | menuA == EmJogo =let (Personagem { aplicaDano = (armado,tempo)}) = (jogador jogoA)
                     in if armado 
                        then return $ primata { jogo = atualiza (acaoInimigos jogoA) (Nothing) jogoA }
                        else return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Descer) jogoA }
  | otherwise = return primata

--------------------------------------------------------------------------------------
-- Input dos Menus 

reage (EventKey (Char '1') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, opcao = opcaoA , imagens = imgsA})
  | menuA == MenuTemas = return $ primata { menu = MenuInicial , tema = temaA}
  | menuA == MenuInicial && opcaoA == Nivel1 = return $ (g initialState1 imgsA){menu = EmJogo, tema = temaA}
  | menuA == MenuInicial && opcaoA == Nivel2 = return $ (g initialState2 imgsA){menu = EmJogo, tema = temaA}
  | menuA == MenuMorte =  return $ (g initialState1 imgsA){menu = EmJogo, tema = temaA}
  | menuA == MenuNivel  = return $ (g initialState2 imgsA){ menu = EmJogo , tema = temaA , opcao = Nivel1}
  | menuA == GG = return $ (g initialState1 imgsA) { menu = MenuInicial , tema = temaA} 
  | otherwise = return primata  
  where getJogo primata = jogo primata


reage (EventKey (Char '2') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema=temaA, imagens = imgsA, opcao = opcaoA})
  | menuA == MenuInicial = return $ primata { menu = MenuNivel } 
  | menuA == MenuMorte =  return $ (g initialState1 imgsA){menu = MenuInicial, tema = temaA}
  | menuA == MenuTemas = return $ primata { menu = MenuInicial , tema = 0}
  | menuA == MenuNivel  = return $ (g initialState2 imgsA){ menu = EmJogo , tema = temaA , opcao = Nivel2}
  | menuA == GG && opcaoA == Nivel1 = return $ (g initialState2 imgsA){ menu = EmJogo , tema = temaA , opcao = Nivel2}
  | menuA == GG && opcaoA == Nivel2 = return $ (g initialState2 imgsA){ menu = MenuInicial , tema = temaA , opcao = Nivel1} 
  | otherwise = return primata 

reage (EventKey (Char '3') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema=temaA, imagens = imgsA})
  | menuA == MenuInicial = return $ primata{ menu = MenuTemas}
  | menuA == MenuTemas = return $ primata { menu = MenuInicial , tema = 1}
  | menuA == MenuNivel = return $ primata { menu = MenuInicial}
  | otherwise = return primata  

--------------------------------------------------------------------------------------------------------------
-- Cheat Codes

--incrementar um ponto ao jogador
reage (EventKey (Char 'p') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, opcao = opcaoA , imagens = imgsA})
  = return $ daPonto primata

--incrementar uma vida ao jogador
reage (EventKey (Char 'v') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, opcao = opcaoA , imagens = imgsA})
  = return $ daVida primata

--teleporta o jogador para a princesa com invencibilidade ao Macaco
reage (EventKey (Char 'g') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, opcao = opcaoA , imagens = imgsA})
  = return $ acabaOJogo primata

--teleporta o jogador para a princesa com invencibilidade ao Macaco
reage (EventKey (Char 'm') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, opcao = opcaoA , imagens = imgsA})
  = return $ daMartelo primata  

-- mata o jogador
reage (EventKey (Char 'f') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, opcao = opcaoA , imagens = imgsA})
  = return $ mataJogador primata 

reage _ primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) Nothing jogoA }

daPonto primata@(PrimateKong{jogo = jogo'@(Jogo{jogador = j@(Personagem{pontos = pontos'})})}) = primata{jogo = jogo'{jogador = j{pontos = pontos'+1}}}
daVida primata@(PrimateKong{jogo = jogo'@(Jogo{jogador = j@(Personagem{vida = vida'})})}) = primata{jogo = jogo'{jogador = j{vida = vida'+1}}}
acabaOJogo primata@(PrimateKong{jogo = jogo'@(Jogo{jogador = j@(Personagem{vida = vida'}), mapa = (Mapa i (fx,fy) matriz)})}) = primata{jogo = jogo'{jogador = j{posicao = (fx,fy), invincibilidade = 10}}}
daMartelo primata@(PrimateKong{jogo = jogo'@(Jogo{jogador = j@(Personagem{aplicaDano = (armado,tempo)})})}) = primata{jogo = jogo'{jogador = j{aplicaDano = (True,600)}}}
mataJogador primata@(PrimateKong{jogo = jogo'@(Jogo{jogador = j@(Personagem{vida = vida'})})}) = primata{jogo = jogo'{jogador = j{vida = 0}}}

------------------------------------------------------------------------------------------------
--Calcula as acoes dos inimigos 

acaoInimigos :: Jogo -> [Maybe Acao]
acaoInimigos (Jogo {inimigos = []}) = []
acaoInimigos jogo@(Jogo { inimigos = ini@(Personagem {posicao = (x,y), velocidade = (xv,yv), direcao = dir, querSaltar = quer , emEscada = emEsc}):t, mapa = mapa@(Mapa a b matriz)})
 | colisoesBordasInimigos ini mapa && not emEsc && not quer = (oposta dir):acaoInimigos jogo{inimigos = t}
 | quer && colideTopoEscada (concat matriz) ini  = (Just Descer) : acaoInimigos jogo { inimigos= t} 
 | quer && not (yv == -50 ) = (Just Subir):acaoInimigos jogo{inimigos = t}
-- | not $ colideEscada (concat matriz) ini && quer = (Just Parar):acaoInimigos jogo{inimigos = t}
 | otherwise = Nothing:acaoInimigos jogo{inimigos = t}

--funcao auxiliar de acaoInimigos para quando o inimigo colide com uma borda a qual ele rebate este ande na direcao contraria
oposta :: Direcao -> Maybe Acao
oposta Este = Just AndarEsquerda
oposta Oeste = Just AndarDireita
oposta Norte = Nothing
oposta Sul = Nothing

--Funcao auxiliar utilizada para o EstadoInical do Jogo devido a erros 
--nos tipos de IO Picture 

--g :: (Jogo,Menu,Opcoes) -> IO Imagens -> PrimateKong  
g (jogo,menu,opcoes,time, tema) imgs = (PrimateKong jogo menu opcoes time tema imgs)
 
 -------------------------------------------------------------
