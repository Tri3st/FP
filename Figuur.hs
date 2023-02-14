------------------------------------------------------------------
-- Functioneel programmeren
-- Opdracht 1: Functionele figuren (versie 1)
--
-- Student: Martin van Diest
-- Nummer: 852192418
--
------------------------------------------------------------------
module Figuur where

import Data.Char
import System.IO

type Figuur a = Pos -> a

type Pos = (Double, Double) -- (x, y)

schaakbord :: Figuur Bool
schaakbord (x, y) = even (round x) == even (round y)

------------------------------------------------------------------
-- 1. ASCII rendering

type Dimensie = (Int, Int) -- (breedte, hoogte)

coordinaten :: Dimensie -> [[(Int, Int)]]
coordinaten dim = [[(x,y) | x <- [0..b - 1]] | y <- [0..h-1]]
  where
    b = fst dim
    h = snd dim

render :: Dimensie -> Figuur a -> [[a]]
render dim fig = [[fig (naarPositie dim x) | x <- xs] | xs <- crdn]
  where
    crdn = coordinaten dim

naarPositie :: Dimensie -> (Int, Int) -> Pos
naarPositie d (x, y) = (fromIntegral x*2/b-1, 1-fromIntegral y*2/h)
  where
    b  = fromIntegral (fst d-1)
    h  = fromIntegral (snd d-1)

boolChar :: Bool -> Char
boolChar c  = if c then '@' else '.'

verander :: (a -> b) -> Figuur a -> Figuur b
verander f x = f . x

toon :: Figuur Bool -> IO ()
toon = putStrLn . unlines . render (64, 40) . verander boolChar
------------------------------------------------------------------
-- 2. Basisvormen

cirkel :: Double -> Figuur Bool
cirkel r (x,y) = x^2 + y^2 <= r^2

vierkant :: Double -> Figuur Bool
vierkant r (x,y) = abs x <= r / 2 && abs y <= r / 2

driehoek :: Figuur Bool
driehoek (x,y) = abs x <= ((-y) + 1) / 2 && y >= (-1)

------------------------------------------------------------------
-- 3. Transformaties

transform :: (Pos -> Pos) -> Figuur a -> Figuur a
transform f x = x . f

verschuif :: (Double, Double) -> Figuur a -> Figuur a
verschuif (dx,dy) = transform (\(x,y) -> (x - dx, y - dy))

schaal :: Double -> Double -> Figuur a -> Figuur a
schaal dx dy = transform (\(x, y) -> (x / dx,y / dy))

infixr 7 #
infixr 7 //

(#) :: (Double, Double) -> Figuur a -> Figuur a -- verschuiven
(#) = verschuif

(//) :: Double -> Figuur a -> Figuur a -- schalen
(//) x = schaal x x

------------------------------------------------------------------
-- 4. Transformaties met poolcoordinaten

type Polar = (Double, Double) -- (afstand, hoek)

toPolar :: Pos -> Polar
toPolar (x, y) = (sqrt (x^2 + y^2), hoek)
  where hoek  | x == 0     = pi/2 * signum y
              | x < 0      = atan (y/x) + pi
              | otherwise  = atan (y/x)

fromPolar :: Polar -> Pos
fromPolar (r, h) = (cos h*r, sin h*r)

transPolar :: (Polar -> Polar) -> Figuur a -> Figuur a
transPolar f figuur = transform (fromPolar . f) figuur . toPolar

roteer :: Double -> Figuur a -> Figuur a
roteer da = transPolar (\(r,a) -> (r, a + da))

krul :: Double -> Figuur a -> Figuur a
krul d = transPolar (\(r,a) -> (r, a - d * r))

------------------------------------------------------------------
-- 5. Composities

compositie :: (a -> b -> c) -> Figuur a -> Figuur b -> Figuur c
compositie func figA figB pos = func (figA pos) (figB pos)

(<+>) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<+>) = compositie (||)

(<&>) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<&>) = compositie (&&)

(<->) :: Figuur Bool -> Figuur Bool -> Figuur Bool
(<->) = compositie (<)

ring :: Double -> Double -> Figuur Bool
ring r1 r2 = (<->) (cirkel r1) (cirkel r2)

box :: Double -> Double -> Figuur Bool
box r1 r2 = (<->) (vierkant r1) (vierkant r2)

------------------------------------------------------------------
-- 6. Kleuren

type Kleur = (Double, Double, Double, Double) -- (rood, groen, blauw, alpha)

alpha :: Kleur -> Double
alpha (_, _, _, a) = a

rood, groen, blauw, zwart, wit, leeg :: Kleur
rood   = (1,0,0,1)
groen  = (0,1,0,1)
blauw  = (0,0,1,1)
zwart  = (0,0,0,1)
wit    = (1,1,1,1)
leeg   = (0,0,0,0) -- volledig doorzichtig

veranderKleur :: (Double -> Double) -> Kleur -> Kleur
veranderKleur d (x, y, z, a) = (d x, d y, d z, d a)

transparant :: Double -> Kleur -> Kleur
transparant d = veranderKleur (* d)

zipKleur :: (Double -> Double -> Double) -> Kleur -> Kleur -> Kleur
zipKleur func (x1,y1,z1,a1) (x2,y2,z2,a2) = (func x1 x2, func y1 y2, func z1 z2, func a1 a2)

mixKleur :: Double -> Kleur -> Kleur -> Kleur
mixKleur dx kleur1 kleur2 = zipKleur (+) (veranderKleur (* dx) kleur1) (veranderKleur (* (1 - dx)) kleur2)

------------------------------------------------------------------
-- 7. PPM rendering

headerPPM :: Dimensie -> String
headerPPM (x,y) = concat ["P6 ",showx," ",showy," 255\n"]
  where
    showx = show x
    showy = show y

kleurPPM :: Kleur -> String
kleurPPM (r,g,b,_) = map (chr . kleurInt) [r,g,b]
    where
      kleurInt x = round (x * 255)

maakPPM :: Dimensie -> Figuur Kleur -> String
maakPPM dim figuur = headerPPM dim ++ concatMap kleurPPM (concat figuren)
  where
    figuren = render dim figuur

schrijf :: FilePath -> Figuur Kleur -> IO ()
schrijf file = writeBinaryFile file . maakPPM (300, 300)

writeBinaryFile :: FilePath -> String -> IO ()
writeBinaryFile file s = do
  h <- openBinaryFile file WriteMode
  hPutStr h s
  hClose h

kleur :: Kleur -> Figuur Bool -> Figuur Kleur
kleur = kleurMet . const


kleurMet :: Figuur Kleur -> Figuur Bool -> Figuur Kleur
kleurMet = compositie (\k b -> if b then k else leeg)

------------------------------------------------------------------
-- 8. Kleuren composities

over :: Kleur -> Kleur -> Kleur
over (x1, y1, z1, a1) (x2, y2, z2, a2) = (x, y, z, a)
  where
    a = a1 + (a2 * (1 - a1))
    x = x1 + (x2 * (1 - a1))
    y = y1 + (y2 * (1 - a1))
    z = z1 + (z2 * (1 - a1))

(<#>) :: Figuur Kleur -> Figuur Kleur -> Figuur Kleur
(<#>) = compositie over

stapel :: [Figuur Kleur] -> Figuur Kleur
stapel = foldr (<#>) (const leeg)
  where
    leeg = (0, 0, 0, 0)
    
------------------------------------------------------------------
-- 9. Gradienten

gradient :: Kleur -> Kleur -> Figuur Kleur
gradient (x1, y1, z1, a1) (x2, y2, z2, a2) pos = (x, y, z, a)
  where
    (x, y, z, a) = (x1 + (x2 - x1) * dx, y1 + (y2 - y1) * dx, z1 + (z2 - z1) * dx, a1 + (a2 - a1) * dx)
    dx = (fst pos + 1) / 2

gradientCirkel :: Kleur -> Kleur -> Figuur Kleur
gradientCirkel  (x1, y1, z1, a1) (x2, y2, z2, a2) (x, y) = (x1 + ((x2 - x1) * (1 - dx)), y1 + ((y2 - y1) * (1 - dx)), z1 + ((z2 - z1) * (1 - dx)), a1 + ((a2 - a1) * (1 - dx)))
  where
    dx = fst (toPolar (x, y))

------------------------------------------------------------------
-- 10. Voorbeelden

fig1, fig2, fig3, fig4 :: Figuur Kleur
fig1 = kleur (transparant 0.6 rood) (cirkel 0.9)
       <#> kleur wit (0.4 // schaakbord)
fig2 = kleur (transparant 0.6 (mixKleur 0.5 blauw wit)) ((0.4,0.4) # vierkant 1)
       <#> kleur rood (cirkel 0.5)
fig3 = stapel [ kleur (transparant 0.8 rood) ((x/2,-0.3) # ring 0.3 0.45)
              | x <- [-1..1] ]
       <#> kleur wit (0.4 // schaakbord <&> driehoek)
fig4 = kleurMet ((0.2,0.2) # 1.2 // gradientCirkel zwart wit) (cirkel 0.8)

type Serie = Double -> Figuur Kleur

serie :: Double -> Serie -> Figuur Kleur
serie n f = stapel (map f [0..n])

spiraal :: Serie -> Serie
spiraal g t = verschuif (fromPolar (t/10, t*0.8)) (g t)

eindvb :: Figuur Kleur
eindvb = figs <#> bollen <#> grid
  where
    paars   =  transparant 0.9 (mixKleur 0.5 rood blauw)
    lgrijs  =  mixKleur 0.3 zwart wit
    bol     =  kleurMet ((0.3,0.3) # 1.2 // gradientCirkel paars rood) (cirkel 1)
    bollen  =  (-0.1, -0.1) # serie 9 (spiraal (\t -> (0.05 + t/40) // bol))
    grid    =  kleurMet achter (0.3 // krul 0.1 schaakbord)
    achter  =  roteer (pi/6) (gradient zwart lgrijs)
    figs    =  (-0.5,0.5) # kleurMet grad (0.15 // foldr1 (<+>) lijst)
    lijst   =  [  (3,-0.5)  # schaal 1.1 0.9 (ring 0.6 1)
               ,  (2,2)     # ring 0.4 1
               ,  (-2,-2)   # roteer (pi/4) (box 0.6 1.5)
               ,  (0,2)     # cirkel 1 <-> ((0.5,0) # cirkel 1)
               ,  roteer (-pi/4) (driehoek <&> cirkel 1)
               ]
    grad    =  gradient (transparant 0.85 groen) (transparant 0.85 blauw)
