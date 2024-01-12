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

type Imagens = [(Imagem,Picture)]

data Imagem
  = MarioD1
  | MarioE1   
  | MarioD2
  | MarioE2
  | MarioC
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
-- --------
  | Bloco
  | EscadaI
  | MoedaI
  | GhostD
  | GhostE
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
  deriving (Show, Eq)

data Menu = EmJogo | MenuInicial | MenuMorte |MenuTemas |MenuNivel
    deriving (Show,Eq)

data Opcoes = Jogar | Sair
    deriving (Show,Eq)
data PrimateKong = PrimateKong { jogo :: Jogo
                               , menu :: Menu
                               , opcao :: Opcoes
                               , timer :: Int
                               , tema :: Int
                               , imagens :: Imagens
                               }



window :: Display
window = InWindow "Teste1" (largura, altura) (0, 0)

inimigoTeste = Personagem (50,0) Fantasma (100,0) Este (1,1) True True 2 0 (False, 0.0) False 0

initialState :: (Jogo,Menu,Opcoes,Int,Int)
initialState = ((Jogo mapa2 [inimigoTeste] listaColecionaveis jogador5), MenuInicial, Jogar, 0, 0) 


largura, altura :: Int
largura = 600
altura = 800

base :: Float
base = -200

mudarTema :: Int -> Int
mudarTema 0 = 1
mudarTema 1 = 0

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
  marioMCD <- loadBMP "MarioMarteloBitD.bmp" 
  marioMCE <- loadBMP "MarioMarteloBitE.bmp" 
  marioMBD <- loadBMP "MarioMarteloBaixoBitD.bmp" 
  marioMBE <- loadBMP "MarioMarteloBaixoBitE.bmp" 
  martelo <- loadBMP "MarteloBit.bmp"
  menu <- loadBMP "MenuBit.bmp" 
  menuMorte <- loadBMP "TelaMorrerBit.bmp"
  menuT <- loadBMP "MenuTemaBit.bmp"
  menuN <- loadBMP "MenuNivelBit.bmp"
  return
    [ (MarioD1, marioD1)
    , (MarioE1, marioE1)
    , (MarioD2, marioD2)
    , (MarioE2, marioE2)
    , (MarioMarteloCimaD , marioMCD)
    , (MarioMarteloCimaE , marioMCE)
    , (MarioMarteloBaixoD , marioMBD)
    , (MarioMarteloBaixoE , marioMBE)
    --, (MarioC, marioC)
    -- tema Hollow
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
    , (MarteloI,martelo)
    , (MenuMorteI,menuMorte)
    , (Menu,menu)
    , (MenuT , menuT)
    , (MenuN, menuN)
    ]

getImagem :: Imagem -> Imagens -> Picture
getImagem key dicionario = fromJust $ lookup key dicionario


draw :: PrimateKong -> IO Picture
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuMorte opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1.1 1.1 $ getImagem MenuMorteI imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuInicial opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1 1 $ getImagem Menu imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuTemas opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1 1 $ getImagem MenuT imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuNivel opcao timer tema imagens)  = 
  return $ Translate 0 0 $ Scale 1 1$ getImagem MenuN imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) EmJogo opcao timer tema imagens)  = 
  return $ Pictures
  [ drawMap mapaD colecionaveisD tema imagens 
   --, Translate (xPos world) (yPos world) $ color red $ rectangleSolid characterWidht characterHeight
  --, desenhaEscada imgs (stairsCoords mapa [(0,0)]) PARA QUE ISTO ? TA A DESENHAR OUTRA VEZ ACHO EU
  --, drawEnemies imgs (enemiesList enemies)
  , drawPrincesa imagens (-250,310)
  , drawInimigos tema imagens inimigosD
    --then Translate (-150) 0 $ color black $ Scale 0.5 0.5 $ Text "Game Over"
    --else drawMario imgs world
  , drawPontos imagens jogadorD
  , if tema == 0 
    then drawMario imagens jogadorD
    else drawHollow imagens jogadorD
  --, Translate (realToFrac $ fst(posicao jogadorD)) (realToFrac $ snd(posicao jogadorD)) $ color red $ rectangleSolid 30 40
    ]

drawPrincesa :: Imagens -> Posicao -> Picture
drawPrincesa imgs (x,y) =  Translate (realToFrac x) ((realToFrac y) - 2) $ Scale 0.9 0.9 $ getImagem Princesa imgs
                                                               

-- tema default
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

-- tema hollow
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


drawInimigos :: Int -> Imagens -> [Personagem] -> Picture
drawInimigos _ _ [] = blank
drawInimigos tema imgs (inimigo@(Personagem {posicao = (x,y)}):t) =
  pictures [drawInimigosAux tema imgs inimigo
  ,drawInimigos tema imgs t] 


drawInimigosAux :: Int -> Imagens -> Personagem -> Picture
drawInimigosAux tema imgs (Personagem { posicao = (x,y), direcao = dir }) | (dir == Oeste) =  
  if tema == 0 then Translate (realToFrac x) ((realToFrac y) - 5) $ Scale 0.9 0.9 $ getImagem GhostE imgs
  else Translate (realToFrac x) ((realToFrac y) + 5) $ Scale 0.45 0.45 $ getImagem HollowFantasmaL imgs
                                                                     | otherwise = 
                                                                      if tema == 0 then Translate (realToFrac x) ((realToFrac y) - 5) $ Scale 0.9 0.9 $ getImagem GhostD imgs
                                                                      else Translate (realToFrac x) ((realToFrac y) + 5) $ Scale 0.45 0.45 $ getImagem HollowFantasmaR imgs



drawMap :: Mapa -> [(Colecionavel,Posicao)] -> Int -> Imagens -> Picture
drawMap (Mapa (posI, dir) posF matriz) listaCol tema imgs = Pictures [
  if tema == 0 
  then Translate 0 0 $ Scale 1 1 $ (getImagem Fundo imgs)
  else Translate 0 0 $ Scale 1 1 $ (getImagem HollowFundo imgs)
  , drawStairs imgs (concat matriz)
  , drawBlocks tema imgs (concat matriz)
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

drawBlocks ::  Int -> Imagens -> [Bloco] -> Picture
drawBlocks _ _ [] = blank  
drawBlocks tema imgs ((Plataforma (x,y)) : rest) =
  pictures [drawBlocksAux tema imgs (Plataforma (x,y)), drawBlocks tema imgs rest]
drawBlocks tema imgs (bloco:rest) = drawBlocks tema imgs rest


drawBlocksAux ::  Int -> Imagens -> Bloco -> Picture
drawBlocksAux tema imgs (Plataforma (x,y)) = 
  if tema == 0 then Translate (realToFrac x) (realToFrac y) $ Scale 1.38 1.38 $ (getImagem Bloco imgs)
  else Translate (realToFrac x) (realToFrac y) $ Scale 0.6 0.6 $ (getImagem HollowPlat imgs)



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
drawColecionavel imgs ((Martelo, (x,y)): rest) = 
  pictures [drawColecionavelAux imgs (Martelo, (x,y)), drawColecionavel imgs rest] 
drawColecionavel imgs (h:t) = drawColecionavel imgs t


drawColecionavelAux ::  Imagens -> (Colecionavel,Posicao) -> Picture
drawColecionavelAux imgs (Moeda, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 1 1.1 $ (getImagem MoedaI imgs)
drawColecionavelAux imgs (Martelo, (x,y)) = Translate (realToFrac x) (realToFrac y+10) $ Scale 0.9 0.9 $ (getImagem MarteloI imgs)
                                       




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

reage (EventKey (Char '1') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema = temaA, imagens = imgsA})
  | menuA == MenuTemas = return $ primata { menu = MenuInicial , tema = temaA}
  | menuA == MenuInicial = return $ (PrimateKong (Jogo mapa2 [inimigoTeste] listaColecionaveis jogador5) EmJogo Jogar 0 temaA imgsA ) 
  | menuA == MenuMorte = return $ (PrimateKong (Jogo mapa2 [inimigoTeste] listaColecionaveis jogador5) EmJogo Jogar 0 temaA imgsA )  
  | otherwise = return primata  

reage (EventKey (Char '2') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema=temaA, imagens = imgsA})
  | menuA == MenuInicial = return $ primata { menu = MenuNivel } 
  | menuA == MenuMorte = return $ primata { menu = MenuInicial } 
  | menuA == MenuTemas = return $ primata { menu = MenuInicial , tema = 0}
  | otherwise = return primata 

reage (EventKey (Char '3') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA ,tema=temaA, imagens = imgsA})
  | menuA == MenuInicial = return $ (PrimateKong (Jogo mapa2 [inimigoTeste] listaColecionaveis jogador5) MenuTemas Jogar 0 (mudarTema temaA) imgsA )
  | menuA == MenuTemas = return $ primata { menu = MenuInicial , tema = 1}
  | menuA == MenuNivel = return $ primata { menu = MenuInicial}
  | otherwise = return primata  

reage _ primata@(PrimateKong { jogo = jogoA  }) = return $ primata { jogo = atualiza (acaoInimigos jogoA) Nothing jogoA }

acaoInimigos :: Jogo -> [Maybe Acao]
acaoInimigos (Jogo {inimigos = []}) = []
acaoInimigos jogo@(Jogo { inimigos = ini@(Personagem {posicao = (x,y), velocidade = (xv,yv), direcao = dir, querSaltar = quer}):t, mapa = mapa@(Mapa a b matriz)})
 | colisoesBordasInimigos ini mapa = (oposta dir):acaoInimigos jogo{inimigos = t}
 | quer = (Just Subir):acaoInimigos jogo{inimigos = t}
-- | not $ colideEscada (concat matriz) ini && quer = (Just Parar):acaoInimigos jogo{inimigos = t}
 | otherwise = Nothing:acaoInimigos jogo{inimigos = t}

adicionarInimigos :: RandomGen g => g -> [Personagem] -> Bool -> [Personagem]
adicionarInimigos gen lista True =
    let (x, gen1) = randomR (-280, 280) gen
        (y, gen2) = randomR (0, 3) gen1
        pos' = par x y
        newInimigo = Personagem { vida = 1, pontos = 0, aplicaDano = (False, 90), querSaltar = False, ressalta = True, tamanho = (30, 40), posicao = ((realToFrac (fst pos')), (realToFrac (snd pos'))), tipo = Fantasma, velocidade = (50, 0), direcao = Este, invincibilidade = 0 }
    in (lista ++ [newInimigo])
adicionarInimigos gen lista False = lista

par :: Float -> Float -> (Float, Float)
par x y = (fromIntegral (floor x) :: Float, fromIntegral (enemiesY (floor y)) :: Float)
  where
    enemiesY 0 = -340
    enemiesY 1 = -180
    enemiesY 2 = -20
    enemiesY 3 = 140
    enemiesY 4 = 300



{-
gerarAleatorioPos :: Int -> Int
gerarAleatorioPos posicao = let n' = head(geraAleatorios (floor posicao) 1)
                            in if (n' > 279) && (n' < -279) 
                               then gerarAleatorioPos posicao 
                               else n'-}


inimigo2 :: Personagem
inimigo2 = Personagem
  { velocidade = (0, 0)
  , tipo       = Jogador
  , posicao    = (0, 350)
  , direcao    = Este
  , tamanho    = (30, 40)
  , emEscada   = False
  , ressalta   = False
  , vida       = 5
  , pontos     = 0
  , aplicaDano = (True, 90)
  , querSaltar = (False)
  }

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
   g (jogo,menu,opcoes,time, tema) imgs = (PrimateKong jogo menu opcoes time tema imgs)
 --g :: (Jogo,Menu,Opcoes) -> IO Imagens -> PrimateKong   


spawnarInimigo :: Int -> Bool
spawnarInimigo time = (mod time 360) == 0   

atualizaPrimata :: Float -> PrimateKong -> IO PrimateKong 
atualizaPrimata dt primata@(PrimateKong jogoA@(Jogo mapa inimigos colecionaveis jogador) menuA opcaoA timer temaA imgsA) = do 
  let jogoA' = if (menuA == EmJogo) 
               then movimenta sementeValor (realToFrac dt) jogoAux
               else jogoA
      gen = (SementeR ((round(fst(posicao jogador)))*(round(snd(posicao jogador))))) 
      jogoAux = jogoA{inimigos = atualizaInimigos (acaoInimigos jogoA) (adicionarInimigos gen inimigos (spawnarInimigo timer)) }
      (Mapa (posI,dirI) posf matriz) = (mapa)
      timer' = timer+1
      menuA' = if (vida jogador) == 999 
               then MenuMorte 
               else menuA
      p = vida jogador
  putStrLn (show p)
  return (PrimateKong jogoA' menuA' opcaoA timer' temaA imgsA )             

