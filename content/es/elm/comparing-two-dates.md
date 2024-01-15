---
title:                "Comparando dos fechas"
html_title:           "Elm: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por Qué?

Comparar dos fechas puede parecer una tarea simple, pero en realidad puede ser bastante complicado. A menudo, tenemos que realizar cálculos o comprobar si una fecha es mayor o menor que otra. En este artículo te enseñaremos cómo hacerlo de manera eficiente en Elm.

## Cómo Hacerlo

La forma más sencilla de comparar dos fechas en Elm es utilizando la función `Date.compare`. Esta función toma dos fechas como argumentos y devuelve un `Order` que representa si la primera fecha es menor, igual o mayor que la segunda fecha. Veamos un ejemplo:

```Elm
Date.compare (Date.fromCalendarDate 2020 01 01) (Date.fromCalendarDate 2020 01 31)
```

Este código devuelve una orden `LT`, que significa que la primera fecha es menor que la segunda. Veamos otro ejemplo:

```Elm
Date.compare (Date.fromCalendarDate 1995 12 24) (Date.fromCalendarDate 2021 04 15)
```

En este caso, la función devuelve una orden `GT`, ya que la primera fecha es mayor que la segunda. Si las dos fechas son iguales, la función devolverá una orden `EQ`.

## Profundizando

Además de la función `Date.compare`, Elm también nos ofrece otras herramientas útiles para trabajar con fechas. Por ejemplo, podemos utilizar la función `Date.add` para sumar o restar días, meses o años a una fecha determinada. También podemos utilizar la función `Date.isBefore` para comprobar si una fecha es anterior a otra.

Otra herramienta interesante es la biblioteca `elm-time`. Esta biblioteca nos permite realizar cálculos más complejos con fechas, como por ejemplo obtener la diferencia entre dos fechas o convertir una fecha a otro huso horario.

## Ver También

A continuación, te dejamos algunos enlaces relacionados con el tema que pueden ser de tu interés:

- [Documentación de la biblioteca `elm-time`](https://package.elm-lang.org/packages/elm/time/latest/)
- [Tutorial de Elm sobre fechas y tiempo](https://dev.to/madnbay/tutorial-elm-dates-and-time-1dn)
- [Libro Electronico "Programming Elm" - capítulo sobre fechas y tiempo](https://elmprogramming.com/dates-and-times.html)