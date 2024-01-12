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
  deriving (Show, Eq)

data Menu = EmJogo | MenuInicial | MenuMorte
    deriving (Show,Eq)

data Opcoes = Jogar | Sair
    deriving (Show,Eq)
data PrimateKong = PrimateKong { jogo :: Jogo
                               , menu :: Menu
                               , opcao :: Opcoes
                               , timer :: Int
                               , imagens :: Imagens
                               }



window :: Display
window = InWindow "Teste1" (largura, altura) (0, 0)

inimigoTeste = Personagem (50,0) Fantasma (100,0) Este (1,1) True True 2 0 (False, 0.0) False 0

initialState :: (Jogo,Menu,Opcoes,Int)
initialState = ((Jogo mapa2 [inimigoTeste] listaColecionaveis jogador5), MenuInicial, Jogar, 0) 


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
  marioMCD <- loadBMP "MarioMarteloBitD.bmp" 
  marioMCE <- loadBMP "MarioMarteloBitE.bmp" 
  marioMBD <- loadBMP "MarioMarteloBaixoBitD.bmp" 
  marioMBE <- loadBMP "MarioMarteloBaixoBitE.bmp" 
  martelo <- loadBMP "MarteloBit.bmp"
  menu <- loadBMP "Menu.bmp" 
  menuMorte <- loadBMP "TelaMorrerBit.bmp"
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
    ]

getImagem :: Imagem -> Imagens -> Picture
getImagem key dicionario = fromJust $ lookup key dicionario


draw :: PrimateKong -> IO Picture
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuMorte opcao timer  imagens)  = 
  return $ Translate 0 0 $ Scale 1.1 1.1 $ getImagem MenuMorteI imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) MenuInicial opcao timer  imagens)  = 
  return $ Translate 0 0 $ Scale 0.9 0.9 $ getImagem Menu imagens
draw (PrimateKong (Jogo { mapa = mapaD , inimigos = inimigosD , colecionaveis = colecionaveisD , jogador = jogadorD}) EmJogo opcao timer  imagens)  = 
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
  , Translate (realToFrac $ fst(posicao jogadorD)) (realToFrac $ snd(posicao jogadorD)) $ color red $ rectangleSolid 30 40
    ]

drawPrincesa :: Imagens -> Posicao -> Picture
drawPrincesa imgs (x,y) =  Translate (realToFrac x) ((realToFrac y) - 2) $ Scale 0.9 0.9 $ getImagem Princesa imgs
                                                               


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
 
reage (EventKey (SpecialKey KeyDown) Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA })
  | menuA == EmJogo =let (Personagem { aplicaDano = (armado,tempo)}) = (jogador jogoA)
                     in if armado 
                        then return $ primata { jogo = atualiza (acaoInimigos jogoA) (Nothing) jogoA }
                        else return $ primata { jogo = atualiza (acaoInimigos jogoA) (Just Descer) jogoA }
  | otherwise = return primata

reage (EventKey (Char '1') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA , imagens = imgsA})
  | menuA == MenuInicial = return $ (PrimateKong (Jogo mapa2 [inimigoTeste] listaColecionaveis jogador5) EmJogo Jogar 0 imgsA ) 
  | menuA == MenuMorte = return $ (PrimateKong (Jogo mapa2 [inimigoTeste] listaColecionaveis jogador5) EmJogo Jogar 0 imgsA )  
  | otherwise = return primata  

reage (EventKey (Char '2') Down _ _) primata@(PrimateKong { jogo = jogoA , menu = menuA , imagens = imgsA})
  | menuA == MenuMorte = return $ primata { menu = MenuInicial  } 
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
   g (jogo,menu,opcoes,time) imgs = (PrimateKong jogo menu opcoes time imgs)
 --g :: (Jogo,Menu,Opcoes) -> IO Imagens -> PrimateKong   


spawnarInimigo :: Int -> Bool
spawnarInimigo time = (mod time 360) == 0   

atualizaPrimata :: Float -> PrimateKong -> IO PrimateKong 
atualizaPrimata dt primata@(PrimateKong jogoA@(Jogo mapa inimigos colecionaveis jogador) menuA opcaoA timer imgsA) = do 
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
  return (PrimateKong jogoA' menuA' opcaoA timer' imgsA )             

