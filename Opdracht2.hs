module Opdracht2 where

import System.Random

type Symbol = Dobbelsteen

newtype Dobbelsteen = Dobbelsteen { waarde :: Int }
  deriving (Eq, Ord, Show)

getDobbelsteen :: Dobbelsteen -> Int
getDobbelsteen worp = worp

data Speler = Speler { naam :: String, symbol :: Symbol }
  deriving (Eq, Show)

data Spel = Spel { spelers :: [Speler], beurt :: Int }

myRandomRIO :: Random a => (a, a) -> IO a
myRandomRIO (lo, hi) = getStdRandom (randomR (lo, hi))

worp :: Dobbelsteen -> IO Dobbelsteen
worp = myRandomRIO (1, 6)

doeWorp :: Spel -> IO Spel
doeWorp spel = do
  let speler = spelers spel !! beurt spel
  putStrLn $ "Het is de beurt aan " ++ naam speler
  putStrLn "Druk op een toets om te gooien"
  getChar
  worp <- worp (symbol speler)
  putStrLn $ "Je hebt een " ++ show worp ++ " gegooid"
  let nieuweSpeler = Speler (naam speler) worp
  let nieuweSpelers = take (beurt spel) (spelers spel) ++ [nieuweSpeler] ++ drop (beurt spel + 1) (spelers spel)
  let nieuwSpel = Spel nieuweSpelers (beurt spel + 1)
  if beurt nieuwSpel == length (spelers nieuwSpel) then do
    putStrLn "Het spel is afgelopen"
    putStrLn $ "De winnaar is " ++ naam (maximum (spelers nieuwSpel))
    return nieuwSpel
  else doeWorp nieuwSpel

geldigeScore :: Int -> Bool
geldigeScore x
  | x > 20 = True
  | otherwise = False

toonNaam :: Speler -> String
toonNaam = fst Speler


main :: IO ()
main = do
  putStr "Geef de naam van de eerste speler: "
  naam1 <- getLine
  putStr "Geef de naam van de tweede speler: "
  naam2 <- getLine
  let speler1 = Speler naam1 (Dobbelsteen 0)
  let speler2 = Speler naam2 (Dobbelsteen 0)
  let spel = Spel [speler1, speler2] 0
  doeWorp
