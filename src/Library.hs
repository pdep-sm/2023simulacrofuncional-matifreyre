module Library where
import PdePreludat

{-
Nombre y Apellido: Hugo Simpson
Legajo: 111111111111
-}
--1.a
data Pizza = Pizza {
    ingredientes :: [String],
    tamanio :: Number,
    calorias :: Number
} deriving Show

--1.b
--es una pizza que tiene “salsa”, “mozzarella” y “orégano”, tiene 8 porciones, y tiene 350 caloría
grandeDeMuzza = Pizza {
    ingredientes = ["salsa", "mozzarella", "orégano"],
    tamanio = 8,
    calorias = 350
}

--2
nivelDeSatisfaccion (Pizza ings _ cal)
    | elem "palmito" ings = 0
    | cal < 500 = valorCalorias
    | otherwise = valorCalorias / 2
    where valorCalorias = length ings * 80 

--3
valorDePizza (Pizza ings tam _) =
    120 * length ings * tam

--4.a
nuevoIngrediente ingrediente pizza = 
    | elem ingrediente $ ingredientes pizza = pizza
    | otherwise = pizza { 
                    ingredientes = ingrediente : ingredientes pizza
                    calorias = calorias pizza + 2 * length ingrediente
                }

--4.b
agrandar pizza = pizza { tamanio = min 10 $ tamanio pizza + 2 }

--4.c
mezcladita primeraPizza segundaPizza = 
    segundaPizza {
        ingredientes = mezclaDeIngredientes primeraPizza segundaPizza,
        calorias = calculoCalorias primeraPizza segundaPizza
    }

mezclaDeIngredientes primeraPizza segundaPizza = 
    agregarSinRepetir (ingredientes primeraPizza) (ingredientes segundaPizza)

calculoCalorias primeraPizza segundaPizza =
    calorias primeraPizza / 2 + calorias segundaPizza 

agregarSinRepetir base agregados = 
    foldl agregar base agregados
    where 
        agregar base agregado
            |notElem agregado base = agregado : base
            |otherwise = base

--5
nivelDeSatisfaccionPedido = sum . map nivelDeSatisfaccion


--6
type Pedido = [Pizza]
type Pizzeria = Pedido -> Pedido

--6.a
pizzeriaLosHijosDePato = map (nuevoIngrediente "palmito")

--6.b
pizzeriaElResumen pedido 
    | length pedido == 1 = pedido
    | otherwise = zipWith mezcladita pedido $ tail pedido

pizzeriaElResumen' [pizza] = [pizza]
pizzeriaElResumen' pedido = zipWith mezcladita pedido $ tail pedido

--6.c
pizzeriaEspecial :: Pizza -> Pizzeria
pizzeriaEspecial predilecta = map (mezcladita predilecta)

pizzeriaPescadito :: Pizzeria
pizzeriaPescadito = pizzeriaEspecial pizzaDeAnchoasBasica

pizzaDeAnchoasBasica = Pizza {
    ingredientes = ["salsa", "anchoas"],
    tamanio = 8,
    calorias = 270
}

--6.d
pizzeriaGourmet nivelExquisitez =
    map agrandar . filter (not.deplorable) 
    where deplorable pizza = nivelDeSatisfaccion pizza <= nivelExquisitez
pizzeriaLaJauja = pizzeriaGourmet 399

--7.a
sonDignasDeCalleCorrientes pedido pizzerias =
    filter (mejoraSatisfaccion pedido) pizzerias
    where mejoraSatisfaccion pedido pizzeria =
            nivelDeSatisfaccionPedido pedido < (nivelDeSatisfaccionPedido . pizzeria) pedido

--7.b
mejorPizzeria pedido = maximoSegun (nivelDeSatisfaccionPedido . ($ pedido)) 

maximoSegun f = foldl1 (mayorSegun f)

mayorSegun f a b
    | f a > f b = a
    | otherwise = b

--9
laPizzeriaPredilecta = foldl1 (.)