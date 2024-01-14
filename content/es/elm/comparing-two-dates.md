---
title:    "Elm: Comparando dos fechas"
keywords: ["Elm"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué

 A menudo, en la programación, es necesario comparar dos fechas para determinar si son iguales, una es anterior o posterior a la otra, o incluso la diferencia entre ellas. En Elm, esto se puede lograr fácilmente utilizando funciones y operadores específicos. En esta publicación, aprenderemos cómo comparar dos fechas en Elm y profundizaremos en cómo funcionan estas comparaciones.

## Cómo hacerlo

Para comparar dos fechas en Elm, se pueden utilizar las funciones ```Date.compare``` y ```Date.diff```, junto con los operadores ```==```, ```<```, ```<=```, ```>``` y ```>=```. Veamos algunos ejemplos:

```
import Date exposing (..)

date1 = Date.fromHttp "2021-05-30"
date2 = Date.fromHttp "2021-05-31"

case Date.compare date1 date2 of
  LT -> "La fecha 1 es anterior a la fecha 2"
  EQ -> "Las fechas son iguales"
  GT -> "La fecha 1 es posterior a la fecha 2"

diff = Date.diff date2 date1
-- diff es igual a 1 día en segundos

date3 = Date.fromHttp "2021-05-30"
date4 = Date.fromHttp "2021-05-29"

date3 > date4 -- devuelve True
date4 <= date2 -- devuelve True
```

Además de comparar fechas específicas, también se pueden usar funciones y operadores para comparar fechas actuales con fechas dadas o crear rangos de fechas. Por ejemplo:

```
import Task
import Date exposing (..)
import Html exposing (text)

getTomorrowDate : Task x Date
getTomorrowDate =
  Date.fromTime 0
    |> Task.andThen (\today ->
        Date.add 1 Day today
          |> Task.map toString
    )

view : Program Never
view =
  Html.text
    ( Task.await getTomorrowDate
        |> toString
    )
-- Devuelve "2021-05-31" si se ejecuta en 30 de mayo de 2021
```

## Inmersión profunda

Cuando se comparan fechas en Elm, es importante recordar que las fechas son solo valores y no tienen ningún tipo de comportamiento asociado. Esto significa que, por ejemplo, una fecha no puede "igualar" o "superar" a otra fecha, ya que simplemente son valores almacenados. Además, al comparar fechas, es importante tener en cuenta que las fechas siempre se comparan en función de su hora UTC, no de la zona horaria del usuario. Esto puede afectar los resultados de las comparaciones, especialmente cuando se utilizan funciones como ```Date.fromCalendarDate```.

## Ver también

- Documentación de funciones de fecha en Elm: https://package.elm-lang.org/packages/elm/time/latest/Date
- Ejemplos de comparación de fechas en Elm: https://gist.github.com/exploreelm/726be8f87ca6ac875b05a084fcc8e36b