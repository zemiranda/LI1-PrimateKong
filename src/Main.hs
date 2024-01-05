module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Bitmap
import Data.Maybe
import System.Random
import Tarefa1
import Tarefa2 
import Tarefa3 
import LI12324

window :: Display
window = InWindow "Teste1" (largura, altura) (0,0)

initialState :: Jogo
initialState = (Jogo mapa2 [] [] jogador5 )
      
largura, altura :: Int
largura = 600
altura = 800

base :: Float
base = -200

data Imagem = MarioD | MarioE | MarioC| Bloco | EscadaI | MoedaI | Ghost | Alcapa | AlcapaAberto 
 deriving (Show , Eq)

type Imagens = [(Imagem,Picture)]

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
   return  [(MarioD, marioD), (MarioE, marioE), (MarioC, marioC) ,(EscadaI, escada),(Bloco, bloco),(MoedaI, moeda), (Ghost, ghost) , (Alcapa, alcapa), (AlcapaAberto,alcapaAberto)]


main :: IO ()
main = do
          imgs <- carregarImagens
          playIO 
                window         -- Janela
                (greyN 0.5)    -- Background
                60             -- FrameRate
                (g (initialState, imgs))   -- Estado Inicial
                draw          -- desenha no ecra
                handleInput    -- inputs
                update         -- updated the world
