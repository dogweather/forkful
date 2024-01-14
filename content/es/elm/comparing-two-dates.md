---
title:                "Elm: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

Si estás aprendiendo a programar en Elm, es importante que entiendas cómo comparar dos fechas. Esto te permitirá realizar acciones basadas en la fecha y hora actual de manera efectiva en tus programas. En este artículo, te mostraré cómo comparar fechas en Elm de forma sencilla.

## Cómo hacerlo

Primero, necesitamos tener dos fechas que queramos comparar. Podemos hacer esto utilizando la función `Date.fromTime` para convertir un registro `Time` en una fecha y utilizar `Date.fromString` para crear una fecha a partir de una cadena de texto.

```Elm
import Date exposing (fromString, fromTime)

-- Creamos dos fechas a partir de una cadena de texto y un registro Time
firstDate = Date.fromString "2021-06-01"
secondDate = Date.fromTime { hour = 12, day = 10, month = 05, year = 2021, second = 30 }
```

A continuación, utilizaremos la función `Date.compare` para comparar ambas fechas. Esta función devuelve un valor `Order` que puede ser `LT` (menor que), `EQ` (igual que) o `GT` (mayor que).

```Elm
-- Esta función devuelve la relación entre las dos fechas
compareResult : Order
compareResult = Date.compare firstDate secondDate
```

Podemos utilizar una declaración `case` para evaluar el resultado de la comparación y realizar acciones en función de ello.

```Elm
case compareResult of
  LT ->
    -- firstDate es menor que secondDate, realizamos alguna acción
    "Realizamos alguna acción"
  
  EQ ->
    -- firstDate es igual que secondDate, realizamos otra acción
    "Realizamos otra acción"
  
  GT ->
    -- firstDate es mayor que secondDate, realizamos otra acción diferente
    "Realizamos otra acción diferente"
```

Este es un ejemplo básico de cómo comparar dos fechas en Elm. Ahora, profundizaremos un poco más en cómo funciona la comparación de fechas.

## Profundizando

Cuando comparamos dos fechas en Elm utilizando la función `Date.compare`, en realidad estamos comparando dos registros `Date` que contienen los campos `year`, `month`, `day` y `hour`. Sin embargo, solo se tienen en cuenta `year`, `month` y `day` ya que la hora no es relevante en la comparación de fechas.

Además, es importante tener en cuenta que, al igual que con otras comparaciones en Elm, la comparación de fechas utiliza la implementación de `Ord` de las fechas. Esto significa que si utilizamos fechas personalizadas que implementan su propia versión de `Ord`, la comparación de fechas puede devolver resultados inesperados.

## Véase también

- Documentación oficial de Elm sobre la función `Date.compare`: https://package.elm-lang.org/packages/elm/time/latest/Date#compare
- Respuesta en Stack Overflow sobre cómo comparar fechas en Elm: https://stackoverflow.com/questions/63935256/how-to-compare-two-dates-in-elm