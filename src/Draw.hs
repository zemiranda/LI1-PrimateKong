module Draw where

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
import Reage
--------------------------------------------carregar ficheiros das imagens--------------------------------


carregarImagens :: IO Imagens
carregarImagens = do
  -- tema Default
  marioE1 <- loadBMP "imagens/MarioBitRunE1.bmp"
  marioD1 <- loadBMP "imagens/MarioBitRunD1.bmp"
  marioE2 <- loadBMP "imagens/MarioBitRunE2.bmp"
  marioD2 <- loadBMP "imagens/MarioBitRunD2.bmp"
  -- tema Hollow
  hollowL1 <- loadBMP "imagens/hollowL.bmp"
  hollowR1 <- loadBMP "imagens/hollowR.bmp"
  hollowL2 <- loadBMP "imagens/hollowL2.bmp"
  hollowR2 <- loadBMP "imagens/hollowR2.bmp"
  hollowMR1 <- loadBMP "imagens/HollowNailR1.bmp"
  hollowML1 <- loadBMP "imagens/HollowNailL1.bmp"
  hollowMR2 <- loadBMP "imagens/HollowNailR2.bmp"
  hollowML2 <- loadBMP "imagens/HollowNailL2.bmp"
  hollowPlataforma <- loadBMP "imagens/HollowChao.bmp"
  hollowFundo <- loadBMP "imagens/HollowBG.bmp"
  hollowFantasmaR <- loadBMP "imagens/RadianceR.bmp"
  hollowFantasmaL <- loadBMP "imagens/RadianceL.bmp"
  hollowPrincesa <- loadBMP "imagens/HollowPrincesa.bmp"
  hollowMacacoR <- loadBMP "imagens/HollowMMR.bmp"
  hollowMacacoL <- loadBMP "imagens/HollowMML.bmp"
  hollowAlcapaoA <- loadBMP "imagens/HollowAlA.bmp"
  hollowAlcapaoF <- loadBMP "imagens/HollowAlF.bmp"
  hollowMoeda <- loadBMP "imagens/HollowMoeda.bmp" 
  hollowMartelo <- loadBMP "imagens/HollowMartelo.bmp" 
  escada <- loadBMP "imagens/escadaBit2.bmp"
  bloco <- loadBMP "imagens/blocoBit.bmp"
  moeda <- loadBMP "imagens/moedaBit.bmp"
  ghostD <- loadBMP "imagens/FantasmaBitD.bmp"
  ghostE <- loadBMP "imagens/FantasmaBitE.bmp"
  alcapa <- loadBMP "imagens/alcapaoBit.bmp"
  alcapaAberto <- loadBMP "imagens/alcapaoAbertoBit.bmp"
  fundo <- loadBMP "imagens/FundoBit.bmp"
  minimacaco <- loadBMP "imagens/MiniMacacoBit.bmp"
  macacoMalvado <- loadBMP "imagens/DonkeyKong.bmp"
  macacoMalvadoL <- loadBMP "imagens/DonkeyKongL.bmp"
  princesa <- loadBMP "imagens/PrincesaBit.bmp"
  marioMCD <- loadBMP "imagens/MarioMarteloBitD.bmp" 
  marioMCE <- loadBMP "imagens/MarioMarteloBitE.bmp" 
  marioMBD <- loadBMP "imagens/MarioMarteloBaixoBitD.bmp" 
  marioMBE <- loadBMP "imagens/MarioMarteloBaixoBitE.bmp" 
  martelo <- loadBMP "imagens/MarteloBit.bmp"
  menu <- loadBMP "imagens/MenuBit.bmp" 
  menuMorte <- loadBMP "imagens/TelaMorrerBit.bmp"
  menuT <- loadBMP "imagens/MenuTemaBit.bmp"
  menuN <- loadBMP "imagens/MenuNivelBit.bmp"
  gg <- loadBMP "imagens/MenuVitoriaBit.bmp"
  return
    [ (MarioD1, marioD1)
    , (MarioE1, marioE1)
    , (MarioD2, marioD2)
    , (MarioE2, marioE2)
    , (MarioMarteloCimaD , marioMCD)
    , (MarioMarteloCimaE , marioMCE)
    , (MarioMarteloBaixoD , marioMBD)
    , (MarioMarteloBaixoE , marioMBE)
    , (HollowR1, hollowR1)
    , (HollowL1, hollowL1)
    , (HollowR2, hollowR2)
    , (HollowL2, hollowL2)
    , (HollowMarteloR1 , hollowMR1)
    , (HollowMarteloL1 , hollowML1)
    , (HollowMarteloR2 , hollowMR2)
    , (HollowMarteloL2 , hollowML2)
    , (HollowPlat, hollowPlataforma)
    , (HollowFundo, hollowFundo)
    , (HollowFantasmaR, hollowFantasmaR)
    , (HollowFantasmaL, hollowFantasmaL)
    , (HollowPrincesa, hollowPrincesa)
    , (HollowMacacoR, hollowMacacoR)
    , (HollowMacacoL, hollowMacacoL)
    , (HollowAlcapaoA, hollowAlcapaoA)
    , (HollowAlcapaoF, hollowAlcapaoF)
    , (HollowMoeda, hollowMoeda)
    , (HollowMartelo, hollowMartelo)
    , (EscadaI, escada)
    , (Bloco, bloco)
    , (MoedaI, moeda)
    , (GhostD, ghostD)
    , (GhostE, ghostE)
    , (MacacoMalvadoI , macacoMalvado)
    , (MacacoMalvadoL , macacoMalvadoL)
    , (Alcapa, alcapa)
    , (AlcapaAberto, alcapaAberto)
    , (MoedaI, moeda)
    , (Fundo,fundo)
    , (Princesa,princesa)
    , (MarteloI,martelo)
    , (MenuMorteI,menuMorte)
    , (Menu,menu)
    , (MenuT , menuT)
    , (MenuN, menuN)
    , (GGI, gg)
    ]

getImagem :: Imagem -> Imagens -> Picture
getImagem key dicionario = fromJust $ lookup key dicionario

--Desenha o mapa , colecionaveis , inimigos , jogador e pontuacao
draw :: PrimateKong -> IO Picture
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuMorte opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1.1 1.1 $ getImagem MenuMorteI imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuInicial opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1 1 $ getImagem Menu imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuTemas opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1 1 $ getImagem MenuT imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuNivel opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1 1$ getImagem MenuN imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) GG opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1 1$ getImagem GGI imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) EmJogo opcao timer tema imagens)  = 
  return $ Pictures
  [ drawMap mapaD colecionaveisD tema imagens 
  , drawPrincesa tema imagens mapaD
  , drawInimigos tema imagens inimigosD
  , drawPontos imagens jogadorD
  , drawVidas imagens jogadorD
  , if tema == 0 
    then drawMario imagens jogadorD
    else drawHollow imagens jogadorD
  ]

----------------------------------------------------------------
-- desenha princesa
drawPrincesa :: Int -> Imagens -> Mapa -> Picture
drawPrincesa 0 imgs (Mapa _ (x,y) _) =  Translate (realToFrac x) ((realToFrac y) - 2) $ Scale 0.9 0.9 $ getImagem Princesa imgs
drawPrincesa 1 imgs (Mapa _ (x,y) _) =  Translate (realToFrac x) ((realToFrac y) - 2) $ Scale 0.5 0.5 $ getImagem HollowPrincesa imgs
                                                               

-- tema default , personagem principal
drawMario :: Imagens -> Personagem -> Picture
drawMario imgs (Personagem { posicao = (x,y), direcao = dir, aplicaDano = (armado,tempo)}) 
 | armado && (dir == Este)  = Translate ((realToFrac x)+5) ((realToFrac y)+4) $ Scale 1 1 $ 
 (if (mod (round (tempo / 10)) 2 == 0)then getImagem MarioMarteloCimaD imgs else getImagem MarioMarteloBaixoD imgs)
 | armado && (dir == Oeste)  = Translate ((realToFrac x)-5) ((realToFrac y)+4) $ Scale 0.9 0.9 $ 
 (if (mod (round (tempo / 10)) 2 == 0) then getImagem MarioMarteloCimaE imgs else getImagem MarioMarteloBaixoE imgs)
 | (dir == Este) =  Translate (realToFrac x) ((realToFrac y)) $ Scale 0.9 0.9 $ 
 (if  even $ round x then getImagem MarioD2 imgs else getImagem MarioD1 imgs)
 | otherwise = Translate (realToFrac x) ((realToFrac y)) $ Scale 0.9 0.9 $ 
 (if even $ round x then getImagem MarioE2 imgs else getImagem MarioE1 imgs)

-- tema hollow , personagem principal 
drawHollow :: Imagens -> Personagem -> Picture
drawHollow imgs (Personagem { posicao = (x,y), direcao = dir, aplicaDano = (armado,tempo)}) 
 | armado && (dir == Este)  = Translate ((realToFrac x)+5) ((realToFrac y)+4) $ Scale 0.6 0.6 $ 
 (if (mod (round (tempo / 10)) 2 == 0)then getImagem HollowMarteloR1 imgs else getImagem HollowMarteloR2 imgs)
 | armado && (dir == Oeste)  = Translate ((realToFrac x)-5) ((realToFrac y)+4) $ Scale 0.6 0.6 $ 
 (if (mod (round (tempo / 10)) 2 == 0) then getImagem HollowMarteloL1 imgs else getImagem HollowMarteloL2 imgs)
 | (dir == Este) =  Translate (realToFrac x) ((realToFrac y)) $ Scale 0.6 0.6 $ 
 (if  even $ round x then getImagem HollowR2 imgs else getImagem HollowR1 imgs)
 | otherwise = Translate (realToFrac x) ((realToFrac y)) $ Scale 0.6 0.6 $ 
 (if even $ round x then getImagem HollowL2 imgs else getImagem HollowL1 imgs)

----------------------------------------------------------------------------------------
-- percorre a lista de inimigos e desenha cada um 
drawInimigos :: Int -> Imagens -> [Personagem] -> Picture
drawInimigos _ _ [] = blank
drawInimigos tema imgs (inimigo@(Personagem {posicao = (x,y)}):t) =
  pictures [drawInimigosAux tema imgs inimigo
  ,drawInimigos tema imgs t] 


drawInimigosAux :: Int -> Imagens -> Personagem -> Picture
--desenha o MacacoMalvado

drawInimigosAux tema imgs (Personagem { posicao = (x,y), direcao = dir , tipo = MacacoMalvado }) 
  | (dir == Oeste) =  
    if tema == 0 then Translate (realToFrac x) ((realToFrac y) +20) $ Scale 0.9 0.9 $ getImagem MacacoMalvadoL imgs
    else Translate (realToFrac x) ((realToFrac y) + 23) $ Scale 0.35 0.35 $ getImagem HollowMacacoL imgs
  | otherwise = 
    if tema == 0 then Translate (realToFrac x) ((realToFrac y) +20) $ Scale 0.9 0.9 $ getImagem MacacoMalvadoI imgs
    else Translate (realToFrac x) ((realToFrac y) + 23) $ Scale 0.35 0.35 $ getImagem HollowMacacoR imgs
--desenha o Fantasma
drawInimigosAux tema imgs (Personagem { posicao = (x,y), direcao = dir }) 
  | (dir == Oeste) =  
    if tema == 0 then Translate (realToFrac x) ((realToFrac y) - 5) $ Scale 0.9 0.9 $ getImagem GhostE imgs
    else Translate (realToFrac x) ((realToFrac y) + 5) $ Scale 0.45 0.45 $ getImagem HollowFantasmaL imgs
  | otherwise = 
    if tema == 0 then Translate (realToFrac x) ((realToFrac y) - 5) $ Scale 0.9 0.9 $ getImagem GhostD imgs
    else Translate (realToFrac x) ((realToFrac y) + 5) $ Scale 0.45 0.45 $ getImagem HollowFantasmaR imgs

-------------------------------------------------------------------------------------------------
--funcao para desenhar os varios componentes do mapa 

drawMap :: Mapa -> [(Colecionavel,Posicao)] -> Int -> Imagens -> Picture
drawMap (Mapa (posI, dir) posF matriz) listaColecionaveis tema imgs = Pictures [
   if tema == 0 
   then Translate 0 0 $ Scale 1 1 $ (getImagem Fundo imgs)
   else Translate 0 0 $ Scale 1 1 $ (getImagem HollowFundo imgs)
   , drawStairs imgs (concat matriz)
   , drawBlocks tema imgs (concat matriz)
   , drawAlcapao tema imgs (concat matriz)
   , drawColecionavel tema imgs listaColecionaveis
  ]

-------------------------------------------------------------------------
--desenha pontos e vidas 

drawPontos :: Imagens -> Personagem -> Picture
drawPontos imgs (Personagem{pontos = p}) =
  Translate 170 370 $ color white $ Scale 0.2 0.2 $ Text $ ("pontos: " ++ show p) 

drawVidas :: Imagens -> Personagem -> Picture
drawVidas imgs (Personagem{vida = p}) =
  Translate 65 370 $ color white $ Scale 0.2 0.2 $ Text $ ("vidas: " ++ show p)

-----------------------------------------------------------------------
--percorre a lista e desenha os blocos 

drawBlocks ::  Int -> Imagens -> [Bloco] -> Picture
drawBlocks _ _ [] = blank  
drawBlocks tema imgs ((Plataforma (x,y)) : rest) =
  pictures [drawBlocksAux tema imgs (Plataforma (x,y)), drawBlocks tema imgs rest]
drawBlocks tema imgs (bloco:rest) = drawBlocks tema imgs rest

drawBlocksAux ::  Int -> Imagens -> Bloco -> Picture
drawBlocksAux tema imgs (Plataforma (x,y)) = 
  if tema == 0 then Translate (realToFrac x) (realToFrac y) $ Scale 1.38 1.38 $ (getImagem Bloco imgs)
  else Translate (realToFrac x) (realToFrac y) $ Scale 0.6 0.6 $ (getImagem HollowPlat imgs)

-----------------------------------------------------------------------
--percorre a lista e desenha as escadas

drawStairs ::  Imagens -> [Bloco] -> Picture
drawStairs _ [] = blank  
drawStairs imgs ((Escada (x,y))  : rest) =
  pictures [drawStairsAux imgs (Escada (x,y)), drawStairs imgs rest]
drawStairs imgs (bloco:rest) = drawStairs imgs rest

 
drawStairsAux ::   Imagens -> Bloco -> Picture
drawStairsAux imgs (Escada (x,y)) = Translate (realToFrac x) (realToFrac y) $ Scale 1 1.1 $ (getImagem EscadaI imgs)

------------------------------------------------------------------------
--percorre a lista e desenha os alcapoes

drawAlcapao :: Int ->  Imagens -> [Bloco] -> Picture
drawAlcapao _ _ [] = blank  
drawAlcapao tema imgs ((Alcapao (x,y) existe tempo): rest) =
  pictures [drawAlcapaoAux tema imgs (Alcapao (x,y) existe tempo), drawAlcapao tema imgs rest]
drawAlcapao tema imgs (bloco:rest) = drawAlcapao tema imgs rest  


drawAlcapaoAux :: Int ->  Imagens -> Bloco -> Picture
drawAlcapaoAux 0 imgs (Alcapao (x,y) existe tempo)|existe = Translate (realToFrac x) (realToFrac y) $ Scale 1 0.9 $ (getImagem AlcapaAberto imgs)
                                                  |otherwise = Translate (realToFrac x) (realToFrac y+13) $ Scale 1 1 $ (getImagem Alcapa imgs)
drawAlcapaoAux 1 imgs (Alcapao (x,y) existe tempo)|existe = Translate (realToFrac x-20) (realToFrac y-10) $ Scale 0.3 0.3 $ (getImagem HollowAlcapaoA imgs)
                                                  |otherwise = Translate (realToFrac x) (realToFrac y+13) $ Scale 0.3 0.3 $ (getImagem HollowAlcapaoF imgs)

---------------------------------------------------------------------------
--percorre a lista e desenha os colecionaveis

drawColecionavel :: Int ->  Imagens -> [(Colecionavel,Posicao)] -> Picture
drawColecionavel _ _ [] = blank  
drawColecionavel tema imgs ((Moeda, (x,y)): rest) =
  pictures [drawColecionavelAux tema imgs (Moeda, (x,y)), drawColecionavel tema imgs rest]
drawColecionavel tema imgs ((Martelo, (x,y)): rest) = 
  pictures [drawColecionavelAux tema imgs (Martelo, (x,y)), drawColecionavel tema imgs rest] 



drawColecionavelAux :: Int -> Imagens -> (Colecionavel,Posicao) -> Picture
drawColecionavelAux 0 imgs (Moeda, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 1 1.1 $ (getImagem MoedaI imgs)
drawColecionavelAux 0 imgs (Martelo, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 0.9 0.9 $ (getImagem MarteloI imgs)
drawColecionavelAux 1 imgs (Moeda, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 0.3 0.3 $ (getImagem HollowMoeda imgs)
drawColecionavelAux 1 imgs (Martelo, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 0.4 0.4 $ (getImagem HollowMartelo imgs)
                                                                              