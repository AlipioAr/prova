
import Prelude
import Data.Char(ord)


func :: Int -> Int
func n = if even n
    then div n 2
    else div (n-1) 2 * (-1)
    
igual :: Int -> Int -> Bool
igual x y = x == y
    
soma :: Int -> Char -> Float
soma i c = fromIntegral (i + ord c)
    
rudecheck :: Char -> Char -> Bool
rudecheck a b = ord b >= 65 && ord b <= 90 && ord a >= 65 && ord a <= 90
    
divisao :: Int -> Int -> String
divisao x y = if y /= 0
    then show ((div x y)*3)
    else "Nao da pra dividir por 0"

ordem :: Char -> Char -> Char
ordem a b = if ord a < ord b
    then a
    else b

areaC :: Float -> Float
areaC r = pi *r*r 

areaQ :: Float -> Float
areaQ l = l*l

digCheck :: Char -> Bool
digCheck x = x>='0' && x< '9'

dif :: Int -> Int -> Bool
dif x y = x/=y 

iguais :: Int -> Int -> Int -> Bool
iguais z x y = not ((z==y) && (x==z) && (x==y))

type Nums = (Int , Int)
soma2:: Nums -> Int
soma2 (x,y) = x+y

type aluno = String
type nota = Float
type juncao  = [(aluno, nota)]

notas :: juncao -> juncao