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
  = MarioD1
  | MarioE1   
  | MarioD2
  | MarioE2
  | MarioC
  | Bloco
  | EscadaI
  | MoedaI
  | GhostD
  | GhostE
  | Alcapa
  | AlcapaAberto
  | Fundo
  | Princesa
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

inimigoTeste = Personagem (-50,0) Fantasma (100,0) Oeste (1,1) True True 2 0 (False, 0.0) False

initialState :: (Jogo,Menu,Opcoes)
initialState = ((Jogo mapa2 [inimigoTeste] listaColecionaveis jogador5), EmJogo, Jogar) 


largura, altura :: Int
largura = 600
altura = 800

base :: Float
base = -200

carregarImagens :: IO Imagens
carregarImagens = do
  marioE1 <- loadBMP "MarioBitRunE1.bmp"
  marioD1 <- loadBMP "MarioBitRunD1.bmp"
  marioE2 <- loadBMP "MarioBitRunE2.bmp"
  marioD2 <- loadBMP "MarioBitRunD2.bmp"
  --marioC <- loadBMP "MarioCBit.bmp"
  escada <- loadBMP "escadaBit2.bmp"
  bloco <- loadBMP "blocoBit.bmp"
  moeda <- loadBMP "moedaBit.bmp"
  ghostD <- loadBMP "FantasmaBitD.bmp"
  ghostE <- loadBMP "FantasmaBitE.bmp"
  alcapa <- loadBMP "alcapaoBit.bmp"
  alcapaAberto <- loadBMP "alcapaoAbertoBit.bmp"
  fundo <- loadBMP "FundoBit.bmp"
  minimacaco <- loadBMP "MiniMacacoBit.bmp"
  princesa <- loadBMP "PrincesaBit.bmp"
  return
    [ (MarioD1, marioD1)
    , (MarioE1, marioE1)
    , (MarioD2, marioD2)
    , (MarioE2, marioE2)
    --, (MarioC, marioC)
    , (EscadaI, escada)
    , (Bloco, bloco)
    , (MoedaI, moeda)
    , (GhostD, ghostD)
    , (GhostE, ghostE)
    , (Alcapa, alcapa)
    , (AlcapaAberto, alcapaAberto)
    , (MoedaI, moeda)
    , (Fundo,fundo)
    , (Princesa,princesa)
    ]

getImagem :: Imagem -> Imagens -> Picture
getImagem key dicionario = fromJust $ lookup key dicionario


draw :: PrimateKong -> IO Picture
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) EmJogo Jogar imagens )  = 
  return $ Pictures
  [ drawMap mapaD colecionaveisD imagens 
   --, Translate (xPos world) (yPos world) $ color red $ rectangleSolid characterWidht characterHeight
  --, desenhaEscada imgs (stairsCoords mapa [(0,0)]) PARA QUE ISTO ? TA A DESENHAR OUTRA VEZ ACHO EU
  --, drawEnemies imgs (enemiesList enemies)
  ,drawPrincesa imagens (-250,310)
  , drawInimigos imagens inimigosD
    --then Translate (-150) 0 $ color black $ Scale 0.5 0.5 $ Text "Game Over"
    --else drawMario imgs world
  , drawPontos imagens jogadorD
  , drawMario imagens jogadorD
    ]

drawPrincesa :: Imagens -> Posicao -> Picture
drawPrincesa imgs (x,y) =  Translate (realToFrac x) ((realToFrac y) - 2) $ Scale 0.9 0.9 $ getImagem Princesa imgs
                                                               


drawMario :: Imagens -> Personagem -> Picture
drawMario imgs (Personagem { posicao = (x,y), direcao = dir }) 
 | (dir == Este) =  Translate (realToFrac x) ((realToFrac y) - 5) $ Scale 0.9 0.9 $ 
 (if  even $ round x then getImagem MarioD2 imgs else getImagem MarioD1 imgs)
 | otherwise = Translate (realToFrac x) ((realToFrac y) - 5) $ Scale 0.9 0.9 $ 
 (if even $ round x then getImagem MarioE2 imgs else getImagem MarioE1 imgs)

drawInimigos :: Imagens -> [Personagem] -> Picture
drawInimigos _ [] = blank
drawInimigos imgs (inimigo@(Personagem {posicao = (x,y)}):t) =
  pictures [drawInimigosAux imgs inimigo
  ,drawInimigos imgs t] 


drawInimigosAux :: Imagens -> Personagem -> Picture
drawInimigosAux imgs (Personagem { posicao = (x,y), direcao = dir }) | (dir == Oeste) =  Translate (realToFrac x) ((realToFrac y) - 5) $ Scale 0.9 0.9 $ getImagem GhostE imgs
                                                                     | otherwise = Translate (realToFrac x) ((realToFrac y) - 5) $ Scale 0.9 0.9 $ getImagem GhostD imgs



drawMap :: Mapa -> [(Colecionavel,Posicao)] -> Imagens -> Picture
drawMap (Mapa (posI, dir) posF matriz) listaCol imgs = Pictures [
  Translate 0 0 $ Scale 1 1 $ (getImagem Fundo imgs)
  , drawStairs imgs (concat matriz)
  , drawBlocks imgs (concat matriz)
  , drawAlcapao imgs (concat matriz)
  , drawColecionavel imgs listaCol
  {-
  , Translate 0 (300) $ color black $ rectangleSolid 10000 1 -- linha
  , Translate 0 (-340) $ color black $ rectangleSolid 10000 1 -- linha
  , Translate 0 (-180) $ color black $ rectangleSolid 10000 1 -- linha
  , Translate 0 (-20) $ color black $ rectangleSolid 10000 1 -- linha
  , Translate 0 (140) $ color black $ rectangleSolid 10000 1 -- linha
  -}
  ]

drawPontos :: Imagens -> Personagem -> Picture
drawPontos imgs (Personagem{pontos = p}) =
  Translate 225 350 $ color white $ Scale 0.4 0.4 $ Text $ show p

drawBlocks ::  Imagens -> [Bloco] -> Picture
drawBlocks _ [] = blank  
drawBlocks imgs ((Plataforma (x,y)) : rest) =
  pictures [drawBlocksAux imgs (Plataforma (x,y)), drawBlocks imgs rest]
drawBlocks imgs (bloco:rest) = drawBlocks imgs rest


drawBlocksAux ::  Imagens -> Bloco -> Picture
drawBlocksAux imgs (Plataforma (x,y)) = Translate (realToFrac x) (realToFrac y) $ Scale 1.38 1.38 $ (getImagem Bloco imgs)




drawStairs ::  Imagens -> [Bloco] -> Picture
drawStairs _ [] = blank  
drawStairs imgs ((Escada (x,y))  : rest) =
  pictures [drawStairsAux imgs (Escada (x,y)), drawStairs imgs rest]
drawStairs imgs (bloco:rest) = drawStairs imgs rest

 
drawStairsAux ::   Imagens -> Bloco -> Picture
drawStairsAux imgs (Escada (x,y)) = Translate (realToFrac x) (realToFrac y) $ Scale 1 1.1 $ (getImagem EscadaI imgs)



drawAlcapao ::  Imagens -> [Bloco] -> Picture
drawAlcapao _ [] = blank  
drawAlcapao imgs ((Alcapao (x,y) existe tempo): rest) =
  pictures [drawAlcapaoAux imgs (Alcapao (x,y) existe tempo), drawAlcapao imgs rest]
drawAlcapao imgs (bloco:rest) = drawAlcapao imgs rest  


drawAlcapaoAux ::  Imagens -> Bloco -> Picture
drawAlcapaoAux imgs (Alcapao (x,y) existe tempo)|existe = Translate (realToFrac x) (realToFrac y) $ Scale 1 0.9 $ (getImagem AlcapaAberto imgs)
                                          |otherwise = Translate (realToFrac x) (realToFrac y+13) $ Scale 1 1 $ (getImagem Alcapa imgs)



drawColecionavel ::  Imagens -> [(Colecionavel,Posicao)] -> Picture
drawColecionavel _ [] = blank  
drawColecionavel imgs ((Moeda, (x,y)): rest) =
  pictures [drawColecionavelAux imgs (Moeda, (x,y)), drawColecionavel imgs rest]
drawColecionavel imgs (bloco:rest) = drawColecionavel imgs rest  


drawColecionavelAux ::  Imagens -> (Colecionavel,Posicao) -> Picture
drawColecionavelAux imgs (Moeda, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 1 1.1 $ (getImagem MoedaI imgs)
                                       -- = Translate (realToFrac x) (realToFrac y) $ Scale 1 1.1 $ (getImagem AlcapaAberto imgs)




reage :: Event  -> PrimateKong -> IO PrimateKong
reage (EventKey (SpecialKey KeyRight) Down _ _) primata@(PrimateKong { jogo = jogoA  }) = 
  return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just AndarDireita) jogoA }
reage (EventKey (SpecialKey KeyLeft) Down _ _)primata@(PrimateKong { jogo = jogoA  }) = 
  return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just AndarEsquerda) jogoA }
reage (EventKey (SpecialKey KeyRight) Up _ _) primata@(PrimateKong { jogo = jogoA  }) = 
  return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Parar) jogoA }
reage (EventKey (SpecialKey KeyLeft) Up _ _) primata@(PrimateKong { jogo = jogoA  }) = 
  return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Parar) jogoA }
reage (EventKey (SpecialKey KeyUp) Down _ _) primata@(PrimateKong { jogo = jogoA }) = 
  let (Mapa (posI,dirI) posf matriz) = (mapa jogoA)
  in if colideEscada (concat matriz) (jogador jogoA)
     then return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Subir) jogoA }
     else return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Saltar) jogoA }
reage (EventKey (SpecialKey KeyDown) Down _ _) primata@(PrimateKong { jogo = jogoA  }) = 
  return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Descer) jogoA }
reage _ primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = 
  atualiza (acaoInimigos jogoA) Nothing jogoA }

acaoInimigos :: Jogo -> [Maybe Acao]
acaoInimigos (Jogo {inimigos = []}) = []
acaoInimigos jogo@(Jogo { inimigos = ini@(Personagem {posicao = (x,y), velocidade = (xv,yv), direcao = dir, querSaltar = quer}):t, mapa = mapa@(Mapa a b matriz)})
 | colisoesBordasInimigos ini mapa = (oposta dir):acaoInimigos jogo{inimigos = t}
 | colideEscada (concat matriz) ini && quer = (Just Subir):acaoInimigos jogo{inimigos = t}
-- | not $ colideEscada (concat matriz) ini && quer = (Just Parar):acaoInimigos jogo{inimigos = t}
 | otherwise = Nothing:acaoInimigos jogo{inimigos = t}

oposta :: Direcao -> Maybe Acao
oposta Norte = Just Descer
oposta Sul = Just Subir
oposta Este = Just AndarEsquerda
oposta Oeste = Just AndarDireita

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
atualizaPrimata dt primata@(PrimateKong jogoA@(Jogo mapa inimigos colecionaveis jogador) menuA opcaoA imgsA) = do 
  let jogoA' = movimenta 1 (realToFrac dt) jogoAux
      jogoAux = jogoA{inimigos = atualizaInimigos (acaoInimigos jogoA) inimigos }
      (Mapa (posI,dirI) posf matriz) = (mapa)
      p = inimigos
  putStrLn (show p)
  return (PrimateKong jogoA' menuA opcaoA imgsA)                