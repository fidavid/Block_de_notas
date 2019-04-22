{- Punto 1 -}
data Deportista = Gimnasta {nombre::String, energia::Int, equilibrio::Int, flexibilidad::Int, fuerza::Int} deriving Show

{- Punto 2 -}
medialuna::Deportista -> Deportista
medialuna gimnasta = gimnasta {equilibrio = equilibrio gimnasta +5}

rolAdelante:: Int -> Deportista -> Deportista
rolAdelante velocidad gimnasta = gimnasta {energia = energia gimnasta + div velocidad 2}

vertical:: Deportista -> Deportista
vertical gimnasta = gimnasta{fuerza = fuerza gimnasta + 7}

saltoSoga:: Int -> Deportista -> Deportista
saltoSoga saltos gimnasta | saltos >= 4 = gimnasta{energia = calcularEnergia saltos gimnasta, fuerza = fuerza gimnasta + saltos}
                          | otherwise = gimnasta

calcularEnergia:: Int -> Deportista -> Int
calcularEnergia salto gimnasta | energia gimnasta >= div salto 2 = energia gimnasta - div salto 2
                               | otherwise = 0

saltoMortal:: Int -> Int -> Deportista -> Deportista
saltoMortal altura impulso gimnasta = gimnasta{fuerza = fuerza gimnasta + altura, flexibilidad = flexibilidad gimnasta + div impulso 2}

{- Punto 3 -}
sonia:: Deportista
sonia = Gimnasta "Sonia" 3 6 10 5

pedro:: Deportista
pedro = Gimnasta "Pedro" 7 5 5 6

{- Punto 4 -}

{- a)

    Sonia:
        Codigo:  (medialuna.saltoMortal 20 15. rolAdelante 10. rolAdelante 10. saltoSoga 10)sonia
        Resultado:      Gimnasta {nombre = "Sonia", energia = 10, equilibrio = 11, flexibilidad = 17, fuerza = 35}

    Pedro:
        Codigo:  (saltoSoga 4.rolAdelante 15.vertical)pedro
        Resultado:      Gimnasta {nombre = "Pedro", energia = 12, equilibrio = 5, flesibilidad = 5, fuerza = 17}
-}

{-
    b) Para lograr el punto a) se utilizan los conceptos de "Aplicacion parcial" y "Composicion de funciones."
-}