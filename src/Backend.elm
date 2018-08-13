module Backend exposing(..)
import Models exposing(Movie, Preferences)
import String exposing(toUpper,words)
import List exposing (map,any)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************
-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--
--peliculaTienePalabrasClave palabras pelicula = String.contains "Toy" pelicula.title

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula = List.all (flip String.contains (toUpper pelicula.title) << toUpper) (words palabras)

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (igualGenero genero)

igualGenero : String -> Movie -> Bool
igualGenero genero pelicula = List.member genero (pelicula.genre)

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores = List.filter (esATP mostrarSoloMenores)

esATP : Bool -> Movie -> Bool
esATP mostrarSoloMenores pelicula = mostrarSoloMenores && pelicula.forKids

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.reverse << List.sortBy .rating

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = List.map (peliculaLikeada id)

peliculaLikeada id pelicula = 
                            if id == pelicula.id then
                            {pelicula | likes = pelicula.likes + 1}
                            else pelicula

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias =  List.map (cambiarPorcentaje preferencias)

cambiarPorcentaje : Preferences -> Movie -> Movie
cambiarPorcentaje preferencias = trunc100 << palabrasClave preferencias.keywords << generoPredilecto preferencias.genre << actorPreferido preferencias.favoriteActor

trunc100 : Movie -> Movie
trunc100 pelicula = if pelicula.matchPercentage > 100 then
                    {pelicula | matchPercentage = 100}
                    else pelicula

aumentarPorcentaje : Movie -> Int -> Movie
aumentarPorcentaje pelicula aumento = {pelicula | matchPercentage = pelicula.matchPercentage + aumento}

palabrasClave : String -> Movie -> Movie
palabrasClave palabras pelicula = aumentarPorcentaje pelicula (List.sum (List.map (puntoPorPalabraClave (toUpper pelicula.title)) (List.map toUpper (String.words palabras))))

puntoPorPalabraClave : String -> String -> Int
puntoPorPalabraClave pelicula palabraClave = if (member palabraClave (words pelicula)) then 
                                             20 
                                             else 0

generoPredilecto : String -> Movie -> Movie
generoPredilecto genero pelicula = if igualGenero genero pelicula then
                                   aumentarPorcentaje pelicula 60
                                   else pelicula
actorPreferido : String -> Movie -> Movie
actorPreferido actor pelicula = if actua actor pelicula then
                                aumentarPorcentaje pelicula 50
                                else pelicula

actua : String -> Movie -> Bool
actua actor pelicula = List.member actor (pelicula.actors)


