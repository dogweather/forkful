---
title:                "Haskell: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado calcular una fecha en el futuro o en el pasado? Puede que estés planeando un evento o simplemente quieras saber cuándo fue tu último cumpleaños. En cualquier caso, la programación en Haskell tiene una solución sencilla y eficiente para este problema.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, debemos utilizar la función `addDays` de la biblioteca `Data.Time`. Esta función toma como argumentos la cantidad de días a añadir o restar, y la fecha inicial en formato `Day` de Haskell.

Veamos un ejemplo de cómo calcular una fecha en el futuro utilizando `addDays`:

```Haskell
import Data.Time

-- Definimos la fecha inicial
let fecha = fromGregorian 2020 11 16

-- Añadimos 10 días a la fecha inicial
let fechaFutura = addDays 10 fecha

-- Imprimimos el resultado en un formato legible
putStrLn $ "La fecha en 10 días será: " ++ show fechaFutura
-- Salida: La fecha en 10 días será: 2020-11-26
```

También podemos calcular una fecha en el pasado al utilizar un número negativo en la función `addDays`. Veamos un ejemplo:

```Haskell
import Data.Time

-- Definimos la fecha inicial
let fecha = fromGregorian 2020 11 16

-- Restamos 5 días a la fecha inicial
let fechaPasada = addDays (negate 5) fecha

-- Imprimimos el resultado en un formato legible
putStrLn $ "La fecha hace 5 días fue: " ++ show fechaPasada
-- Salida: La fecha hace 5 días fue: 2020-11-11
```

¡Así de sencillo es calcular una fecha en el futuro o en el pasado en Haskell!

## Profundizando

La función `addDays` en realidad utiliza un tipo de datos más general llamado `AddDays`, que puede tomar diferentes tipos de fechas, no sólo `Day`. Por ejemplo, podemos calcular una fecha en el futuro utilizando `LocalTime`, que incluye también la hora. Veamos un ejemplo:

```Haskell
import Data.Time

-- Definimos la hora actual
fechaActual <- getCurrentTime

-- Añadimos una hora
let fechaFutura = addDays 1 fechaActual

-- Imprimimos el resultado en un formato legible
putStrLn $ "Fecha y hora en 1 día: " ++ show fechaFutura
-- Salida: Fecha y hora en 1 día: 2020-11-26 16:38:27.475627 UTC
```

También podemos utilizar la función `addDays` con otros tipos de datos, como `UTCTime` o `UniversalTime`. Si quieres saber más sobre estos tipos de datos y cómo utilizarlos con la función `addDays`, ¡puedes consultar la documentación oficial de Haskell [aquí](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html#g:4)! 

## Ver también

- [Documentación de la biblioteca `Data.Time`](https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html)
- [Tutorial de Haskell en español](https://github.com/carlosazaustre/haskell-tutorial)
- [Introducción a la programación en Haskell](https://www.freecodecamp.org/news/introduction-to-programming-in-haskell/) (en inglés)