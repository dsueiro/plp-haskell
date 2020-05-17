import Data.List
import Test.HUnit

valorAbsoluto :: Float -> Float
valorAbsoluto x = if ( x>=0 ) 
  then x
  else 0-x

divide :: Int -> Int -> Bool
divide d n = (mod n d) == 0

bisiesto :: Int -> Bool
bisiesto n = if (divide 4 n) 
  then (not (divide 100 n)) || (divide 400 n)
  else False

factorial' :: Int -> Int
factorial' n = if (n == 0) then 1 else n * factorial' (n-1)

esPrimo :: Int -> Bool 
esPrimo n = null [ d | d <-[2..n-1],  ((mod n d) == 0)]

divisoresPrimos :: Int -> [Int]
divisoresPrimos n = [x | x <- [1..n], (divide x n) && esPrimo x]

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos = length . divisoresPrimos 


inverso :: Float -> Maybe Float
inverso r = if (r == 0) then Nothing else Just (1/r)

aEntero :: Either Int Bool -> Int
aEntero r = case r of 
        Left x -> x
        Right x -> if x then 1 else 0
limpiar :: String -> String -> String
limpiar letras origen = foldr (\t part -> if (elem t letras) then part else t:part) [] origen

promedio :: [Float] -> Float
promedio lista = if null lista then 0 else (sum lista) / fromIntegral (length lista)

difPromedio :: [Float] -> [Float]
difPromedio lista = map (\x -> x-(promedio lista)) lista

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales x = foldr (\t sonIguales -> (t == head x) && sonIguales) True x 

-- Tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
    "ejercicio2" ~: testsEj2,
    "ejercicio3" ~: testsEj3,
    "ejercicio4" ~: testsEj4
 ]


testsEj2 = test [
  3 ~=? valorAbsoluto 3,
  3 ~=? valorAbsoluto (-3),
  True ~=? bisiesto 1992,
  False ~=? bisiesto 1991,
  False ~=? bisiesto 1990,
  False ~=? bisiesto 1900,
  False ~=? bisiesto 1800,
  False ~=? bisiesto 1700,
  True ~=? bisiesto 1600,
  120 ~=? factorial' 5,
  1 ~=? factorial' 1,
  1 ~=? cantDivisoresPrimos 1,
  2 ~=? cantDivisoresPrimos 2,
  2 ~=? cantDivisoresPrimos 4,
  3 ~=? cantDivisoresPrimos 12,
  4 ~=? cantDivisoresPrimos 30
 ]

testsEj3 = test [
 Just 1 ~=? inverso 1,
 Just 0.5 ~=? inverso 2,
 Nothing ~=? inverso 0,
 1 ~=? aEntero (Right True),
 0 ~=? aEntero (Right False),
 3 ~=? aEntero (Left 3)
 ]

testsEj4 = test [
  "pera" ~=? limpiar "susto" "puerta",
  [-1, 0, 1] ~=? difPromedio [2, 3, 4],
  True ~=? todosIguales [],
  True ~=? todosIguales [1],
  True ~=? todosIguales [1, 1, 1],
  True ~=? todosIguales [3, 3, 3],
  False ~=? todosIguales [2, 1, 1],
  False ~=? todosIguales [2, 2, 1],
  False ~=? todosIguales [2, 1, 2]
 ]
