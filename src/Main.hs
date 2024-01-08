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

type Imagens = [(Imagem,Picture)]

data Imagem
  =  MarioD
  | MarioE
  | MarioC
  | Bloco
  | EscadaI
  | MoedaI
  | Ghost
  | Alcapa
  | AlcapaAberto
  deriving (Show, Eq)

data Menu = EmJogo | MenuInicial
data Opcoes = Jogar | Sair

data PrimateKong = PrimateKong { jogo :: Jogo
                               , menu :: Menu
                               , opcao :: Opcoes
                               , imagens :: Imagens
                               }


window :: Display
window = InWindow "Teste1" (largura, altura) (0, 0)

initialState :: (Jogo,Menu,Opcoes)
initialState = ((Jogo mapa2 [] [] jogador5), EmJogo, Jogar) 


largura, altura :: Int
largura = 600
altura = 800

base :: Float
base = -200

carregarImagens :: IO Imagens
carregarImagens = do
  marioE <- loadBMP "marioBitE.bmp"
  marioD <- loadBMP "marioBit.bmp"
  marioC <- loadBMP "MarioCBit.bmp"
  escada <- loadBMP "escadaBit2.bmp"
  bloco <- loadBMP "blocoBit.bmp"
  moeda <- loadBMP "moedaBit.bmp"
  ghost <- loadBMP "goubaBit.bmp"
  alcapa <- loadBMP "alcapaoBit.bmp"
  alcapaAberto <- loadBMP "alcapaoAbertoBit.bmp"
  return
    [ (MarioD, marioD)
    , (MarioE, marioE)
    , (MarioC, marioC)
    , (EscadaI, escada)
    , (Bloco, bloco)
    , (MoedaI, moeda)
    , (Ghost, ghost)
    , (Alcapa, alcapa)
    , (AlcapaAberto, alcapaAberto)
    ]

getImagem :: Imagem -> Imagens -> Picture
getImagem key dicionario = fromJust $ lookup key dicionario


draw :: PrimateKong -> IO Picture
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) EmJogo Jogar imagens )  = 
  return $ Pictures
  [ drawMap mapaD imagens 
   --, Translate (xPos world) (yPos world) $ color red $ rectangleSolid characterWidht characterHeight
  --, desenhaEscada imgs (stairsCoords mapa [(0,0)]) PARA QUE ISTO ? TA A DESENHAR OUTRA VEZ ACHO EU
  --, drawEnemies imgs (enemiesList enemies)
  , drawMario imagens jogadorD
    --then Translate (-150) 0 $ color black $ Scale 0.5 0.5 $ Text "Game Over"
    --else drawMario imgs world
    ]

drawMario :: Imagens -> Personagem -> Picture
drawMario imgs (Personagem { posicao = (x,y), direcao = dir }) | (dir == Este) =  Translate (realToFrac x) ((realToFrac y) - 3) $ Scale 1.3 1.3 $ getImagem MarioD imgs
                                                               | otherwise = Translate (realToFrac x) ((realToFrac y) - 3) $ Scale 1.3 1.3 $ getImagem MarioE imgs

drawMap :: Mapa ->  Imagens -> Picture
drawMap (Mapa (posI, dir) posF matriz) imgs = Pictures [
    drawStairs imgs (concat matriz)
  , drawBlocks imgs (concat matriz)
  , drawAlcapao imgs (concat matriz)
  , Translate 0 (300) $ color black $ rectangleSolid 10000 1 -- linha
  , Translate 0 (-340) $ color black $ rectangleSolid 10000 1 -- linha
  , Translate 0 (-180) $ color black $ rectangleSolid 10000 1 -- linha
  , Translate 0 (-20) $ color black $ rectangleSolid 10000 1 -- linha
  , Translate 0 (140) $ color black $ rectangleSolid 10000 1 -- linha
  ]


drawBlocks ::  Imagens -> [Bloco] -> Picture
drawBlocks _ [] = blank  
drawBlocks imgs ((Plataforma (x,y)) : rest) =
  pictures [drawBlocksAux imgs (Plataforma (x,y)), drawBlocks imgs rest]
drawBlocks imgs (bloco:rest) = drawBlocks imgs rest


drawBlocksAux ::  Imagens -> Bloco -> Picture
drawBlocksAux imgs (Plataforma (x,y)) = Translate (realToFrac x) (realToFrac y) $ Scale 1.32 1.32 $ (getImagem Bloco imgs)




drawStairs ::  Imagens -> [Bloco] -> Picture
drawStairs _ [] = blank  
drawStairs imgs ((Escada (x,y))  : rest) =
  pictures [drawStairsAux imgs (Escada (x,y)), drawStairs imgs rest]
drawStairs imgs (bloco:rest) = drawStairs imgs rest

 
drawStairsAux ::   Imagens -> Bloco -> Picture
drawStairsAux imgs (Escada (x,y)) = Translate (realToFrac x) (realToFrac y) $ Scale 1 1.1 $ (getImagem EscadaI imgs)



drawAlcapao ::  Imagens -> [Bloco] -> Picture
drawAlcapao _ [] = blank  
drawAlcapao imgs ((Alcapao (x,y) existe ): rest) =
  pictures [drawAlcapaoAux imgs (Alcapao (x,y) existe ), drawAlcapao imgs rest]
drawAlcapao imgs (bloco:rest) = drawAlcapao imgs rest  


drawAlcapaoAux ::  Imagens -> Bloco -> Picture
drawAlcapaoAux imgs (Alcapao (x,y) existe)|existe = Translate (realToFrac x) (realToFrac y) $ Scale 1 1.1 $ (getImagem Alcapa imgs)
                                          |otherwise = Translate (realToFrac x) (realToFrac y) $ Scale 1 1.1 $ (getImagem AlcapaAberto imgs)




reage :: Event  -> PrimateKong -> IO PrimateKong
reage (EventKey (SpecialKey KeyRight) Down _ _) primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just AndarDireita) jogoA }
reage (EventKey (SpecialKey KeyLeft) Down _ _)primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just AndarEsquerda) jogoA }
reage (EventKey (SpecialKey KeyRight) Up _ _) primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Parar) jogoA }
reage (EventKey (SpecialKey KeyLeft) Up _ _) primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Parar) jogoA }
reage (EventKey (SpecialKey KeyUp) Down _ _) primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Saltar) jogoA }
reage _ primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) Nothing jogoA }

acaoInimigos :: Jogo -> [Maybe Acao] 
acaoInimigos (Jogo { inimigos = inimigosA }) = undefined

main :: IO ()
main = do
  imgs <- carregarImagens
  playIO
    window -- Janela
    (greyN 0.5) -- Background
    60 -- FrameRate
    (g (initialState) imgs) -- Estado Inicial
    draw -- desenha no ecra
    reage -- inputs
    atualizaPrimata -- updated the world
 where
   g (jogo,menu,opcoes) imgs = (PrimateKong jogo menu opcoes imgs)
 --g :: (Jogo,Menu,Opcoes) -> IO Imagens -> PrimateKong   

atualizaPrimata :: Float -> PrimateKong -> IO PrimateKong 
atualizaPrimata dt primata@(PrimateKong jogoA menuA opcaoA imgsA) = do 
  let jogoA' = movimenta 1 (realToFrac dt) jogoA
      p = velocidade (jogador jogoA)
      --p = colisoesChao (mapa jogoA) (jogador jogoA)
  putStrLn (show p)
  return (PrimateKong jogoA' menuA opcaoA imgsA)                