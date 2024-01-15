---
title:                "Calculando una fecha en el futuro o en el pasado"
html_title:           "Haskell: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué?

¿Alguna vez has necesitado calcular una fecha en el futuro o en el pasado? Puede que hayas querido planificar un viaje o simplemente saber qué día de la semana caerá tu cumpleaños en el próximo año. ¡Con Haskell, puedes hacerlo de forma fácil y rápida!

## Cómo hacerlo

Primero, necesitamos importar el módulo de fechas de Haskell usando la siguiente línea de código:

```Haskell
import Data.Time
```

A continuación, definimos una función que nos permita calcular una fecha en el futuro o en el pasado. Esta función tomará como parámetros la fecha actual, la cantidad de días que queremos agregar o restar, y el signo (+ o -) para indicar si queremos sumar o restar días.

```Haskell
calculaFecha :: UTCTime -> Int -> Char -> UTCTime
calculaFecha fechaActual numDias signo = addUTCTime (realToFrac (numDias * 86400)) fechaActual
```

Ahora, podemos probar nuestra función con diferentes parámetros para obtener diferentes resultados. Por ejemplo, si queremos saber qué fecha sería dentro de 100 días, podemos hacer lo siguiente:

```Haskell
calculaFecha (UTCTime (fromGregorian 2021 10 5) 0) 100 '+'
```

Esto nos dará una fecha dentro de 100 días a partir del 5 de octubre de 2021. Además, también podemos especificar una fecha en el pasado y obtener una fecha en el futuro si usamos el signo de resta (-).

## Profundizando

¿Cómo funciona exactamente nuestra función? Primero, importamos el módulo de fechas para poder utilizar sus funciones, como addUTCTime. Luego, definimos nuestra función calculaFecha, que toma como parámetro una fecha en formato UTCTime, un número de días y un signo. Dentro de la función, utilizamos la función addUTCTime para sumar o restar la cantidad de segundos equivalentes a la cantidad de días especificados por el usuario. Por ejemplo, si queremos sumar 100 días, multiplicamos 100 por 86400, que es el número de segundos en un día. Finalmente, la función devuelve una nueva fecha en formato UTCTime.

Con esta función, podemos calcular fechas en el futuro o en el pasado de forma rápida y sencilla. Prueba diferentes combinaciones de parámetros para obtener diferentes resultados y juega con las funciones de fechas de Haskell para seguir aprendiendo.

## Ver también

- [Documentación del módulo Data.Time en Haskell](https://hackage.haskell.org/package/time/docs/Data-Time.html)
- [Tutorial de Haskell en español](https://www.haskell.es/tutoriales/tutorial-de-haskell)
- [Ejemplos de código para practicar en Haskell](http://www.science.smith.edu/dftwiki/index.php/Haskell_exercises)