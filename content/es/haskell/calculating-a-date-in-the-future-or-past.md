---
title:                "Calculando una fecha en el futuro o pasado"
html_title:           "Haskell: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Calculando una Fecha en el Futuro o Pasado con Haskell 

## ¿Qué y Por qué?

Calcular una fecha en el futuro o pasado es determinar una fecha específica a partir de una fecha inicial y un intervalo de tiempo. Los programadores lo hacen para hacer seguimiento de eventos, programar recordatorios o calcular la duración de un evento.

## ¿Cómo hacerlo?

En Haskell, podemos usar la biblioteca `Data.Time` para realizar cálculos de fechas. Aquí está la función `addDays` que nos permite sumar un número de días a una fecha:

```Haskell
import Data.Time

calculaFechaFuturo :: Day -> Integer -> Day
calculaFechaFuturo fechaInicial diasSumar =
    addDays diasSumar fechaInicial
```
Usa la función con una fecha y un número de días que quieras agregar. Por ejemplo:

```Haskell
import Data.Time

main = do
    let fecha = fromGregorian 2022 10 07
    putStrLn $ show $ calculaFechaFuturo fecha 5
```

Salida: `2022-10-12`

Esto nos dice que si sumamos cinco días a la fecha 2022-10-07 obtenemos 2022-10-12.

## Más a Fondo

La computación de fechas se ha utilizado en la programación desde los primeros días de las computadoras. Las bibliotecas modernas como `Data.Time` de Haskell han hecho que trabajar con fechas sea mucho más fácil.

Además de agregar días, también podemos agregar meses y años usando las funciones `addGregorianMonthsRollOver` y `addGregorianYearsRollOver` respectivamente. También existe `addUTCTime` para agregar un intervalo de tiempo a una fecha y hora en formato UTC.

Al calcular fechas futuras o pasadas, hay que tener en cuenta los años bisiestos. La biblioteca `Data.Time` de Haskell considera esto por defecto, lo que elimina la necesidad de comprobaciones adicionales.

## Ver también 

1. ['Data.Time' en Hackage](http://hackage.haskell.org/package/time-1.10/docs/Data-Time.html) - Documentación oficial de la biblioteca `Data.Time`.
2. [Calculating Dates in the Future - Stack Overflow](https://stackoverflow.com/questions/46319252/calculating-dates-in-the-future) - Discusión en Stack Overflow sobre cómo calcular fechas en el futuro.
3. [Programming in Haskell by Graham Hutton](http://www.cs.nott.ac.uk/~pszgmh/pih.html) - Un libro de texto excelente para principiantes en Haskell que incluye una discusión sobre el uso de la biblioteca `Data.Time`.