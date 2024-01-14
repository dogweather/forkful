---
title:    "Elm: Comparando dos fechas."
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

Comparar dos fechas es una tarea común en la programación. Ya sea para realizar cálculos de tiempo o para verificar la validez de una fecha ingresada por un usuario, la comparación de fechas es una habilidad esencial en la programación. En este artículo, aprenderemos cómo comparar dos fechas en Elm de manera sencilla y efectiva.

## Cómo hacerlo

Para comparar dos fechas en Elm, utilizaremos la función `compare` del módulo `Date`. Esta función toma dos argumentos de tipo `Date` y devuelve un `Order`, que indica si la primera fecha es menor, igual o mayor que la segunda fecha.

```elm
Imports:

import Date exposing (compare)
import Time exposing (..)

Dates to compare:

date1 : Date
date2 : Date

date1 = fromTime <| posixToMillis 1546300800 -- 1/1/2019 at midnight
date2 = fromTime <| posixToMillis 1577836800 -- 1/1/2020 at midnight

Comparison:

compare date1 date2 -- Result: GT (Greater Than)
```

Podemos ver que, en este ejemplo, la fecha1 (1 de enero de 2019) es mayor que la fecha 2 (1 de enero de 2020).

## Profundizando

Es importante tener en cuenta que la función `compare` solo compara las fechas en sí, no las horas ni los minutos. Esto significa que si queremos comparar fechas con horas y minutos específicos, primero debemos convertirlas a la hora Unix (posix). También podemos utilizar otras funciones del módulo `Date`, como `toHour`, `toMinute` y `toSecond`, para obtener información más detallada de nuestras fechas.

Además, también podemos utilizar la función `max` y `min` del módulo `Basics` para obtener la fecha más reciente o la fecha más antigua entre dos fechas.

## Ver también

- [Documentación de la función `compare` del módulo `Date`](https://package.elm-lang.org/packages/elm/time/latest/Time#compare)
- [Guía de fechas en Elm](https://guide.elm-lang.org/dates_and_times/)
- [Conversión de fechas a formato posix en Elm](https://discourse.elm-lang.org/t/how-can-i-convert-from-elm-date-to-unix-epoch-without-a-pit-of-despair/424)