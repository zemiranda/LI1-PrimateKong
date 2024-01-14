module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss.Interface.IO.Game

import Data.Maybe
import System.Random
import Definicoes
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import LI12324 
import Draw
import Reage

data SementeR = SementeR Int deriving (Show, Eq)

instance RandomGen SementeR where
    next (SementeR s) = let newS = (s * 1103515245 + 12345) `mod` 2147483648
                        in (newS, SementeR newS)


window :: Display
window = InWindow "Primata Kong" (largura, altura) (0, 0)

------------------------------------------------------------------------------------------------------
-- Adiciona inimigos em posicaoes x aleatorias dependentes da semente e da posicao x e y do jogador , de 6 em 6 segundos 
-- atraves do Bool calculado na funcao tempoNascerInimigo , usando o cronometro geral do data PrimataKong
adicionarInimigos :: RandomGen g => Niveis -> g -> [Personagem] -> Bool -> [Personagem]
adicionarInimigos Nivel1 gen lista True =
    let (x, gen1) = randomR (-270, 270) gen
        (y, gen2) = randomR (0, 3) gen1
        pos' = par x y
        newInimigo = Personagem { vida = 1, pontos = 0, aplicaDano = (False, 90), querSaltar = False, ressalta = True, tamanho = (30, 40), posicao = ((realToFrac (fst pos')), (realToFrac (snd pos'))), tipo = Fantasma, velocidade = (50, 0), direcao = Este, invincibilidade = 0 , emEscada = False }
    in if length lista > inimigosLimite1 then lista else (lista ++ [newInimigo])
adicionarInimigos Nivel2 gen lista True =
    let (x, gen1) = randomR (-270, 270) gen
        (y, gen2) = randomR (0, 3) gen1
        pos' = par x y
        newInimigo = Personagem { vida = 1, pontos = 0, aplicaDano = (False, 90), querSaltar = False, ressalta = True, tamanho = (30, 40), posicao = ((realToFrac (fst pos')), (realToFrac (snd pos'))), tipo = Fantasma, velocidade = (50, 0), direcao = Este, invincibilidade = 0 , emEscada = False }
    in if length lista > inimigosLimite2 then lista else (lista ++ [newInimigo])
adicionarInimigos nivel gen lista False = lista

podeNascerInimigo :: Int -> Bool
podeNascerInimigo time = (mod time 360) == 0   

par :: Float -> Float -> (Float, Float)
par x y = (fromIntegral (floor x) :: Float, fromIntegral (enemiesY (floor y)) :: Float)
  where
    enemiesY 0 = -340
    enemiesY 1 = -180
    enemiesY 2 = -20
    enemiesY 3 = 140
    enemiesY 4 = 300

----------------------------------------------------------------------------------------------------------
--Atualiza o estado principal PrimataKong

atualizaPrimata :: Float -> PrimateKong -> IO PrimateKong 
atualizaPrimata dt primata@(PrimateKong jogoA@(Jogo mapa@(Mapa i (fx,fy) matriz) inimigos colecionaveis jogador) menuA opcaoA timer temaA imgsA) = do 
  let 
      --gerar um numero aleatorio atraves da posicao x e y do jogador e de uma semente defenida no 
      -- data type SementeR , com instance de RandomGen defenida 
      gen = (SementeR ((round(fst(posicao jogador)))*(round(snd(posicao jogador))))) 
      ----------------------------------------------------
      -- atualizar as acoes dos inimigos e adiconar novos inimigos
      atualizaInmigosJogo = jogoA {inimigos = 
          atualizaInimigos (acaoInimigos jogoA) (adicionarInimigos opcaoA gen inimigos (podeNascerInimigo timer)) }
      ----------------------------------------------------
      --incrementar timer geral 
      timer' = timer+1
      ----------------------------------------------------
      --atualizar os menus quando o jogador morre ou vence
      menuA' = if (vida jogador) == 0 then MenuMorte 
               else if  (fx,fy) == posicaoFinal then GG 
               else menuA
      -----------------------------------------------------
      --Atualizar o jogo ( Jogador, Inimigos, Colecionaveis, Mapa )
      jogoA' = if (menuA == EmJogo) then movimenta sementeValor (realToFrac dt) atualizaInmigosJogo
               else jogoA
      -----------------------------------------------------
    --p = vida jogador 
--putStrLn (show p)
  return (PrimateKong jogoA' menuA' opcaoA timer' temaA imgsA )        

--------------------------------------------------------------------------
--Funcao Main , responsavel por iniciar o jogo 

main :: IO ()
main = do
  imgs <- carregarImagens --carrega as Imagens 
  playIO
    window -- Janela
    (greyN 0) -- Background
    60 -- FrameRate
    (g (initialState1) imgs) -- Estado Inicial
    draw -- desenha no ecra
    reage -- inputs
    atualizaPrimata -- updated the world

-------------------------------------------------------------

