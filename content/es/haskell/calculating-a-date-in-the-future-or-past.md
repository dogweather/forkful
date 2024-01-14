---
title:                "Haskell: Calculando una fecha en el futuro o pasado"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Por qué

Calcular fechas en el futuro o en el pasado es una habilidad útil en la programación, ya que a menudo necesitamos manejar y mostrar fechas de manera dinámica en nuestras aplicaciones. En este artículo, te mostraré cómo puedes hacerlo fácilmente utilizando Haskell. 

## Cómo hacerlo

El primer paso es importar el módulo `Data.Time` que nos proporciona funciones para manejar fechas y horas. Luego, podemos utilizar la función `addDays` para sumar o restar días a una fecha dada. Aquí hay un ejemplo de cómo calcular la fecha de mañana:

```Haskell
import Data.Time

main :: IO()
main = do
    today <- getCurrentTime
    let tomorrow = addDays 1 $ utctDay today
    putStrLn $ "Mañana es " ++ show tomorrow
```

La salida de este código sería algo como: `Mañana es 2021-08-31`. Como puedes ver, utilizamos la función `utctDay` para obtener la fecha actual y luego agregamos un día usando `addDays`.

También podemos sumar o restar meses o años utilizando las funciones `addMonths` y `addYears` respectivamente. Aquí hay un ejemplo de cómo obtener la fecha dentro de un mes:

```Haskell
import Data.Time

main :: IO()
main = do
    today <- getCurrentTime
    let nextMonth = addMonths 1 $ utctDay today
    putStrLn $ "Dentro de un mes es " ++ show nextMonth
```

La salida sería: `Dentro de un mes es 2021-09-30`.

## Profundizando

Si quieres calcular fechas de manera más precisa, puedes utilizar la función `addGregorianMonthsClip` que tiene en cuenta los diferentes días en cada mes. Esto significa que si, por ejemplo, quieres sumar un mes a una fecha que es el 31 de enero, la función ajustará la fecha resultante al 28 de febrero para tener en cuenta la duración de febrero.

También puedes utilizar la función `addGregorianYearsClip` para tener en cuenta los años bisiestos al sumar o restar años a una fecha. 

Además, hay muchas más funciones y tipos de datos disponibles en el módulo `Data.Time` que pueden ayudarte a manejar fechas de manera más avanzada. Te recomiendo revisar su documentación para aprender más sobre ellas.

## Ver también

- [Documentación de `Data.Time`](https://hackage.haskell.org/package/time/docs/Data-Time.html) 
- [Guía de programación de Haskell](https://wiki.haskell.org/Haskell_guide) 
- [Cursos de Haskell en línea](https://www.coursera.org/courses?query=haskell)