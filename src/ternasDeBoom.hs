import Data.Function.Memoize
import qualified Data.Map.Strict as M
main = do  print  ((obtenerSumaDeTernas [1..1000000]))
           return ((obtenerSumaDeTernas [1..1000000]))



insertMany :: Ord k => [(k, v)] -> M.Map k v -> M.Map k v
insertMany = flip (foldr (uncurry M.insert))

sieve :: Int -> [Int]
sieve n = map fst . filter snd $ M.toList (loop initial 2)
  where
    initial = M.fromList ([(0, False), (1, False)] ++ [(i, True) | i <- [2..(n - 1)]])
    loop p i
      | i == n                     = p
      | M.findWithDefault True i p = loop (scrapMultiples i p) (i + 1)
      | otherwise                  = loop p (i + 1)
    scrapMultiples i p = insertMany [(k * i, False) | k <- [2..n `div` i], k * i < n] p

memoizedPrimos= memoize sieve 1000000
esPrimo :: Int -> Bool
esPrimo 1  = False
esPrimo 2  = True
esPrimo n  =  elem   n  memoizedPrimos 

memoizedEsPrimo :: Int->Bool
memoizedEsPrimo unNumero 		= memoize esPrimo unNumero	   
formanPrimo unNumero otroNumero = memoizedEsPrimo (unNumero + otroNumero)

sonTernaDeBoom num1 num2 num3 	= (formanPrimo num1 num2) && (formanPrimo num2 num3)
esParteDeTernaInferior unNumero = sonTernaDeBoom unNumero (unNumero+1) (unNumero+2) 
filtrarTernasDeBoom 			= filter esParteDeTernaInferior 
construirTerna unNumero			= (unNumero,(unNumero+1),(unNumero+2))
contstruirSumaMasPerformante    = (+3) . (*3)
obtenerTernas 					= map construirTerna.filtrarTernasDeBoom
obtenerSumaDeTernas 			= map contstruirSumaMasPerformante.filtrarTernasDeBoom


obtenerPrimosDeTerna (x,y,z)	= (x+y,y+z)
getPrimosFromTernas 			= (map obtenerPrimosDeTerna ).obtenerTernas 


getKMaximo unNumero (otroNumero:numeros)
 | 3 == unNumero					= 0
 | (3*otroNumero + 3) == unNumero 	= otroNumero
 | (3*otroNumero + 3) > unNumero 	= otroNumero-1
 | otherwise 						= getKMaximo unNumero numeros 

esMultiploDe3k3ConKIgualA3n unNumero	= ((==0). mod (getKMaximo unNumero [1..])) 3

--observaciones: cuando busco los primos del 1 al 100 que no estan en los generados por las ternas me encuentro con todos aquellos primos laterales no gemelos
--asi que si puedo estudiar el patron que hay en estas ternas puedo obtener la ubicacion de todos los primso gemelos
--la suma de los numeros de todas las ternas es multiplo de 3 !- duh que gil n + n+1 + n+2 = (3n +3 ) siempre da resto 0
-- si bien los pares de ternas siempre generan 3k+3, los k= 3 n parecen no ser parte de los generados por las ternas-- chequear esto 
-- chequear que todas las sumas de ternas se puedan formar con alguna suma 2  ternas menores al numero
