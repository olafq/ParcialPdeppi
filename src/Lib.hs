--PARCIAL Pdeppi
import Text.Show.Functions
import Data.List
--Parte 1--
-- punto 1 / 2--
data Persona = Persona {
    nombre           :: String,
    direccion        :: Direccion,
    dineroDisponible :: Float,
    comidaFavorita   :: Comida,
    cupones          :: [Cupon]
} deriving Show

data Comida = Comida {
    nombreComida :: String,
    costo        :: Float,
    ingredientes :: [Ingrediente]
} deriving Show
data Direccion = Direccion {
    calle  :: String,
    altura :: Int
} deriving Show
type Ingrediente = String
type Cupon       = Comida -> Comida

mapdineroDisponible :: (Float -> Float) -> Persona -> Persona
mapdineroDisponible unaFuncion unaPersona = unaPersona {dineroDisponible = unaFuncion . dineroDisponible $ unaPersona}
incdineroDisponible :: Float -> Persona -> Persona
incdineroDisponible cantidad  = mapdineroDisponible (+ cantidad) 
decDineroDisponible :: Float -> Persona -> Persona
decDineroDisponible cantidad = mapdineroDisponible (subtract cantidad)
setcomidaFavorita :: Comida -> Persona -> Persona
setcomidaFavorita unaComida unaPersona = unaPersona {comidaFavorita = unaComida}
mapCosto :: (Float -> Float) -> Comida -> Comida
mapCosto unaFuncion unaComida = unaComida {costo = unaFuncion . costo $ unaComida}
incCosto :: Float -> Comida -> Comida
incCosto cantidad = mapCosto (+ cantidad)
decCosto :: Float -> Comida -> Comida
decCosto cantidad = mapCosto (subtract cantidad)
mapNombreComida :: (String -> String) -> Comida -> Comida
mapNombreComida unaFuncion unaComida = unaComida{nombreComida = unaFuncion . nombreComida $ unaComida}
mapIngredientes :: ([Ingrediente] -> [Ingrediente]) -> Comida -> Comida
mapIngredientes unaFuncion unaComida = unaComida {ingredientes = unaFuncion . ingredientes $ unaComida}
agregarIngrediente :: Ingrediente -> Comida -> Comida
agregarIngrediente unIngrediente  = mapIngredientes (unIngrediente :) 
setIngredientes :: [Ingrediente] -> Comida -> Comida
setIngredientes nuevosIngredientes unaComida = unaComida {ingredientes = nuevosIngredientes }
-- punto 3--
paula :: Persona
paula = Persona "Paula"  (Direccion "Thames" 1585) 3600 hamburguesaDeluxe []
hamburguesaDeluxe :: Comida
hamburguesaDeluxe = Comida "hamburguesa deluxe" 350 ["pan","carne", "lechuga" , "tomate" , "panceta", "queso", "huevo frito"]
olaf :: Persona 
olaf = Persona "Olaf" (Direccion "santander" 6) 0 hamburguesaMessi [{-agregar 2 cupones-}]
hamburguesaMessi :: Comida
hamburguesaMessi = Comida "hamburguesa Messi" 100 ["pan", "lechuga" , "tomate" , "panceta", "huevo frito","chedar"]

-- parte 2--
--punto 1--
comprar ::  Comida -> Persona  -> Persona
comprar  unaComida unaPersona 
    | meAlcanzaParaComprar && ((<200) . costo $ unaComida) = setcomidaFavorita unaComida $ cobrarComida  
    | meAlcanzaParaComprar = cobrarComida
    | otherwise = unaPersona
    where precioComida = costo unaComida
          meAlcanzaParaComprar = (dineroDisponible unaPersona) >= (precioComida) 
          cobrarComida = decDineroDisponible (precioComida) unaPersona

-- punto 2-- 
carritoDeCompras :: [Comida] -> Persona -> Persona -- PREGUNTAR SI ESTA BIEN RESUELTO 
carritoDeCompras comidas unaPersona = decDineroDisponible 100   (foldl (\unaPersona unaComida ->comprar unaComida unaPersona ) unaPersona comidas)
-- punto 1--
ingredientesNoVeganos :: [String]
ingredientesNoVeganos = [ "carne", "huevo" , "queso"]
semanaVegana :: Cupon -- Preguntar si ta bien
semanaVegana unaComida 
   | hayIngredientesNoVeganos = decCosto ((costo unaComida) /2) unaComida
   | otherwise = unaComida
   where hayIngredientesNoVeganos = null (intersect  ingredientesNoVeganos (ingredientes unaComida))
--punto 2 -- 
esoNoEsCocaPapi :: Ingrediente -> Cupon
esoNoEsCocaPapi unaBebida  = mapNombreComida (++ "Party") . agregarIngrediente unaBebida  
-- punto 3--
sinTACCis :: Cupon -- PREGUNTAR SI ESTA BIEN 
sinTACCis unaComida =setIngredientes (ingredientesSinGluten unaComida) unaComida
ingredientesSinGluten :: Comida -> [Ingrediente]
ingredientesSinGluten  =map (++ " libre de gluten") . ingredientes 
-- punto 4 --
findeVegetariano :: Cupon
findeVegetariano unaComida 
    | any (=="carne") (ingredientes unaComida) = unaComida
    | otherwise = decCosto (porcientoCosto{-Que nombre se podria poner-Preguntar-}) unaComida
    where porcientoCosto = costo unaComida * 0.3

-- punto 5 --

largaDistancia :: Cupon
largaDistancia = mapIngredientes (filter ((<= 10) . length)) . mapCosto (+ 50)
--Parte c--
--punto 1--
comprarConCupones :: Persona -> Persona
comprarConCupones  unaPersona = comprar  (aplicarCuponesComida  (cupones unaPersona) (comidaFavorita unaPersona)) $ unaPersona

aplicarCuponesComida :: [Cupon] -> Comida -> Comida
aplicarCuponesComida unosCupones unaComida = foldl (\unaComida unCupon -> unCupon unaComida) unaComida unosCupones

--punto 2--

superComida :: [Comida] -> Comida
superComida unasComidas = 
  Comida{
    nombreComida = eliminarVocales . juntarNombres $ unasComidas,
    costo = sumatoriaDePrecios unasComidas,
    ingredientes = eliminarRepetidos . juntarIngredientes $ unasComidas
  }

-- su precio es la sumatoria de todos los precios

sumatoriaDePrecios :: [Comida] -> Float
sumatoriaDePrecios = sum . (map costo)

-- su nombre es el conjunto de todos los nombres sacando las vocales

juntarNombres :: [Comida] -> String
juntarNombres =concat . map (nombreComida) 

eliminarVocales :: String -> String
eliminarVocales = filter (not.esVocal)

esVocal :: Char -> Bool
esVocal letra = elem letra ['a','e','i','u','u']


-- sus ingredientes son todos los ingredientes juntos sin repetidos

juntarIngredientes :: [Comida] -> [String]
juntarIngredientes = concat . (map ingredientes)

eliminarRepetidos :: [String] -> [String]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (filter (/= x) xs)

-- punto 3-- 
comprarDeluxe :: [Comida] -> Persona -> Persona
comprarDeluxe comidas  = comprar $ superComida (duplicarPrecioComidas . comidadMenoresA400 $ comidas) 
comidadMenoresA400 :: [Comida] -> [Comida]
comidadMenoresA400 = filter ((<400) . costo)

duplicarPrecioComidas :: [Comida] -> [Comida]
duplicarPrecioComidas  = map (mapCosto (*2))



