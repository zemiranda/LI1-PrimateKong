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

data SementeR = SementeR Int deriving (Show, Eq)

instance RandomGen SementeR where
    next (SementeR s) = let newS = (s * 1103515245 + 12345) `mod` 2147483648
                        in (newS, SementeR newS)


data Menu = EmJogo | MenuInicial | MenuMorte |MenuTemas |MenuNivel | GG
    deriving (Show,Eq)

data Niveis = Nivel1 | Nivel2
    deriving (Show,Eq)
data PrimateKong = PrimateKong { jogo :: Jogo
                               , menu :: Menu
                               , opcao :: Niveis
                               , timer :: Int
                               , tema :: Int
                               , imagens :: Imagens
                               }



window :: Display
window = InWindow "Primata Kong" (largura, altura) (0, 0)
--------------------------------Configuracao inicial para cada nivel -------------------------------------

initialState1 :: (Jogo,Menu,Niveis,Int,Int)
initialState1 = ((Jogo mapa1Aux listaInimigos listaColecionaveisMapa1 
                 (Personagem
                   { velocidade = (0, 0)
                   , tipo       = Jogador
                   , posicao    = fst $ posInicial mapa1Aux
                   , direcao    = snd $ posInicial mapa1Aux
                   , tamanho    = (30, 40)
                   , emEscada   = False
                   , ressalta   = False
                   , vida       = 5
                   , pontos     = 0
                   , aplicaDano = (False, 0)
                   , querSaltar = (False)
                   , invincibilidade = 0 
                   })), MenuInicial, Nivel1, 0, 0) 

initialState2 :: (Jogo,Menu,Niveis,Int,Int)
initialState2 = ((Jogo mapa2Aux listaInimigos listaColecionaveisMapa2 
                 (Personagem
                   { velocidade = (0, 0)
                   , tipo       = Jogador
                   , posicao    = fst $ posInicial mapa1Aux
                   , direcao    = snd $ posInicial mapa1Aux
                   , tamanho    = (30, 40)
                   , emEscada   = False
                   , ressalta   = False
                   , vida       = 5
                   , pontos     = 0
                   , aplicaDano = (False, 0)
                   , querSaltar = (False)
                   , invincibilidade = 0 
                   })), MenuInicial, Nivel1, 0, 0) 


posInicial (Mapa inicial fim f) = inicial


mudarTema :: Int -> Int
mudarTema 0 = 1
mudarTema 1 = 0

--------------------------------------------carregar ficheiros das imagens--------------------------------

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
  | MacacoMalvadoI
  | MacacoMalvadoL
  | Alcapa
  | AlcapaAberto
  | Fundo
  | Princesa
  | MarteloI
  | MarioMarteloCimaD 
  | MarioMarteloCimaE 
  | MarioMarteloBaixoD 
  | MarioMarteloBaixoE
  | MenuMorteI
  | Menu
  | MenuT
  | MenuN
  | GGI
    -- tema Hollow
  | HollowL1
  | HollowR1
  | HollowL2
  | HollowR2
  | HollowMarteloR1
  | HollowMarteloL1
  | HollowMarteloR2
  | HollowMarteloL2
  | HollowPlat  
  | HollowFundo
  | HollowFantasmaR
  | HollowFantasmaL
  | HollowPrincesa 
  | HollowMacacoR
  | HollowMacacoL
  | HollowAlcapaoA
  | HollowAlcapaoF
  | HollowMoeda 
  | HollowMartelo 
-- --------
  deriving (Show, Eq)


carregarImagens :: IO Imagens
carregarImagens = do
  -- tema Default
  marioE1 <- loadBMP "MarioBitRunE1.bmp"
  marioD1 <- loadBMP "MarioBitRunD1.bmp"
  marioE2 <- loadBMP "MarioBitRunE2.bmp"
  marioD2 <- loadBMP "MarioBitRunD2.bmp"
  -- tema Hollow
  hollowL1 <- loadBMP "hollowL.bmp"
  hollowR1 <- loadBMP "hollowR.bmp"
  hollowL2 <- loadBMP "hollowL2.bmp"
  hollowR2 <- loadBMP "hollowR2.bmp"
  hollowMR1 <- loadBMP "HollowNailR1.bmp"
  hollowML1 <- loadBMP "HollowNailL1.bmp"
  hollowMR2 <- loadBMP "HollowNailR2.bmp"
  hollowML2 <- loadBMP "HollowNailL2.bmp"
  hollowPlataforma <- loadBMP "HollowChao.bmp"
  hollowFundo <- loadBMP "HollowBG.bmp"
  hollowFantasmaR <- loadBMP "RadianceR.bmp"
  hollowFantasmaL <- loadBMP "RadianceL.bmp"
  hollowPrincesa <- loadBMP "HollowPrincesa.bmp"
  hollowMacacoR <- loadBMP "HollowMMR.bmp"
  hollowMacacoL <- loadBMP "HollowMML.bmp"
  hollowAlcapaoA <- loadBMP "HollowAlA.bmp"
  hollowAlcapaoF <- loadBMP "HollowAlF.bmp"
  hollowMoeda <- loadBMP "HollowMoeda.bmp" 
  hollowMartelo <- loadBMP "HollowMartelo.bmp" 
  escada <- loadBMP "escadaBit2.bmp"
  bloco <- loadBMP "blocoBit.bmp"
  moeda <- loadBMP "moedaBit.bmp"
  ghostD <- loadBMP "FantasmaBitD.bmp"
  ghostE <- loadBMP "FantasmaBitE.bmp"
  alcapa <- loadBMP "alcapaoBit.bmp"
  alcapaAberto <- loadBMP "alcapaoAbertoBit.bmp"
  fundo <- loadBMP "FundoBit.bmp"
  minimacaco <- loadBMP "MiniMacacoBit.bmp"
  macacoMalvado <- loadBMP "DonkeyKong.bmp"
  macacoMalvadoL <- loadBMP "DonkeyKongL.bmp"
  princesa <- loadBMP "PrincesaBit.bmp"
  marioMCD <- loadBMP "MarioMarteloBitD.bmp" 
  marioMCE <- loadBMP "MarioMarteloBitE.bmp" 
  marioMBD <- loadBMP "MarioMarteloBaixoBitD.bmp" 
  marioMBE <- loadBMP "MarioMarteloBaixoBitE.bmp" 
  martelo <- loadBMP "MarteloBit.bmp"
  menu <- loadBMP "MenuBit.bmp" 
  menuMorte <- loadBMP "TelaMorrerBit.bmp"
  menuT <- loadBMP "MenuTemaBit.bmp"
  menuN <- loadBMP "MenuNivelBit.bmp"
  gg <- loadBMP "MenuVitoriaBit.bmp"
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
drawColecionavel tema imgs (h:t) = drawColecionavel tema imgs t


drawColecionavelAux :: Int -> Imagens -> (Colecionavel,Posicao) -> Picture
drawColecionavelAux 0 imgs (Moeda, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 1 1.1 $ (getImagem MoedaI imgs)
drawColecionavelAux 0 imgs (Martelo, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 0.9 0.9 $ (getImagem MarteloI imgs)
drawColecionavelAux 1 imgs (Moeda, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 0.3 0.3 $ (getImagem HollowMoeda imgs)
drawColecionavelAux 1 imgs (Martelo, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 0.4 0.4 $ (getImagem HollowMartelo imgs)
                                                                              
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
  | menuA == MenuNivel  = return $ (g initialState2 imgsA){ menu = EmJogo , tema = temaA , opcao = Nivel2}
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


-- Cheat Codes
reage (EventKey (Char 'p') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, opcao = opcaoA , imagens = imgsA})
  = return $ daPonto primata

reage (EventKey (Char 'v') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, opcao = opcaoA , imagens = imgsA})
  = return $ daVida primata

reage _ primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) Nothing jogoA }

daPonto primata@(PrimateKong{jogo = jogo'@(Jogo{jogador = j@(Personagem{pontos = pontos'})})}) = primata{jogo = jogo'{jogador = j{pontos = pontos'+1}}}
daVida primata@(PrimateKong{jogo = jogo'@(Jogo{jogador = j@(Personagem{vida = vida'})})}) = primata{jogo = jogo'{jogador = j{vida = vida'+1}}}
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
                                 atualizaInimigos 
                                  (acaoInimigos jogoA) 
                                  (adicionarInimigos opcaoA gen inimigos (podeNascerInimigo timer)) }
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
--Funcao auxiliar utilizada para o EstadoInical do Jogo devido a erros 
--nos tipos de IO Picture 

--g :: (Jogo,Menu,Opcoes) -> IO Imagens -> PrimateKong  
g (jogo,menu,opcoes,time, tema) imgs = (PrimateKong jogo menu opcoes time tema imgs)
 
 -------------------------------------------------------------

