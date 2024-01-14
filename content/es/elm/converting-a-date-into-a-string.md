---
title:                "Elm: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# ¿Por qué convertir una fecha en una cadena en Elm?

Convertir una fecha en una cadena puede ser útil en varias situaciones, como mostrar la fecha en un formato específico o comparar fechas en una aplicación. En este artículo, exploraremos cómo hacerlo en Elm de manera sencilla y eficiente.

## Cómo hacerlo

En Elm, podemos convertir una fecha en una cadena utilizando la función `toString` del módulo `Date`. Veamos un ejemplo:

```Elm
import Date exposing (toString)
import Time exposing (utc)

date = utc 2021 10 15

toString date -- "2021-10-15"
```

En este ejemplo, importamos los módulos `Date` y `Time` y creamos una fecha utilizando la función `utc` que toma como argumentos el año, mes y día. Luego, utilizamos la función `toString` para convertir la fecha en una cadena con el formato "año-mes-día".

Podemos manipular la fecha antes de convertirla en una cadena. Por ejemplo, si queremos mostrar la fecha en formato "día/mes/año", podemos hacerlo utilizando la función `toDayMonthYear` del módulo `Date`:

```Elm
import Date exposing (toString, toDayMonthYear)
import Time exposing (utc)

date = utc 2021 10 15

toString (toDayMonthYear date) -- "15/10/2021"
```

Otra opción es utilizar la función `fromTime` del módulo `Date` para crear una fecha a partir de un valor de tiempo y luego convertirla en una cadena:

```Elm
import Date exposing (toString, fromTime)
import Time exposing (posixToMillis, utc)

timestamp = posixToMillis 1634300400

date = fromTime timestamp

toString date -- "2021-10-15"
```

## Profundizando en la conversión de fechas a cadenas

Al convertir una fecha en una cadena, es importante tener en cuenta el formato en el que queremos mostrarla. Por ejemplo, si queremos mostrar el nombre del mes en lugar de su número, podemos usar la función `toMonthName` del módulo `Date`:

```Elm
import Date exposing (toString, toDayMonthYear, toMonthName)
import Time exposing (utc)

date = utc 2021 10 15

toString (toDayMonthYear date) -- "15/10/2021"
toString (toMonthName date) -- "October 15, 2021"
```

Además, podemos utilizar la función `toIso8601` del módulo `Date` para obtener la fecha en formato ISO 8601, que es un estándar internacional para mostrar fechas y horas:

```Elm
import Date exposing (toString, toIso8601)
import Time exposing (utc)

date = utc 2021 10 15

toString date -- "2021-10-15"
toString (toIso8601 date) -- "2021-10-15T00:00:00+00:00"
```

En muchos casos, también puede ser útil convertir una fecha en una cadena para realizar comparaciones entre fechas. En Elm, podemos utilizar la función `toTime` del módulo `Date` para obtener la fecha como un valor de tiempo, que luego podemos comparar utilizando operadores como `>`, `<` o `==`:

```Elm
import Date exposing (toTime)

date1 = toTime (utc 2021 10 15)
date2 = toTime (utc 2021 10 20)

date2 > date1 -- True
```

## Ver también

- Documentación oficial de Elm sobre el módulo Date: https://package.elm-lang.org/packages/elm/time/latest/Date
- Artículo sobre cómo manejar fechas y horas en Elm: https://thoughtbot.com/blog/handling-dates-and-times-in-elm