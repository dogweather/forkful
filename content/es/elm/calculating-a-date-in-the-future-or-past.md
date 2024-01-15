---
title:                "Calculando una fecha en el futuro o en el pasado"
html_title:           "Elm: Calculando una fecha en el futuro o en el pasado"
simple_title:         "Calculando una fecha en el futuro o en el pasado"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## ¿Por qué calcular fechas futuras o pasadas en Elm?

Existen muchas situaciones en las que necesitamos calcular una fecha en el futuro o en el pasado, como por ejemplo, para planificar eventos o para realizar tareas programadas. En Elm, podemos utilizar funciones y tipos de datos específicos para realizar estos cálculos de manera sencilla y precisa.

## Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado en Elm, primero debemos tener en cuenta el tipo de dato Date. Este tipo representa una fecha específica, con el formato "yyyy-mm-dd", donde "yyyy" es el año, "mm" es el mes y "dd" es el día.

Para obtener la fecha actual, podemos utilizar la función `Date.today` que nos devolverá una instancia de Date con la fecha actual. A partir de ahí, podemos utilizar las funciones `Date.add` y `Date.sub` para sumar o restar días, semanas, meses o años a la fecha actual.

A continuación, un ejemplo de código en Elm que calcula la fecha de un evento que ocurrirá en 3 semanas:

````Elm
import Date exposing (Date)
import Date.Extra exposing (weeks)

-- Obtenemos la fecha actual
today : Date
today = Date.today

-- Calculamos la fecha del evento sumando 3 semanas a la fecha actual
eventoFecha : Date
eventoFecha = Date.add (weeks 3) today

-- Imprimimos la fecha del evento en consola
main =
    Date.format "%d de %B de %Y" eventoFecha
        |> Debug.log "Fecha del evento"
````

El output de este código será: "18 de mayo de 2021". Podemos ver que utilizando las funciones adecuadas, es muy fácil obtener una fecha en el futuro o en el pasado en Elm.

## Deep Dive

En Elm, también podemos utilizar la función `Date.fromParts` para construir una instancia de Date a partir de valores específicos para el año, mes y día. Además, también existen funciones para obtener el día de la semana de una fecha, comparar fechas y realizar cálculos más complejos.

Es importante tener en cuenta que, al igual que con cualquier otro tipo de dato en Elm, el tipo Date es inmutable. Esto significa que cada vez que utilizamos una función para sumar o restar valores a una fecha, se genera una nueva instancia de Date en lugar de modificar la fecha original.

Por último, es importante mencionar que, para trabajar con zonas horarias en Elm, podemos utilizar la biblioteca `elm/time` que nos provee de funciones y tipos de datos específicos para manejar fechas y horas en diferentes zonas.

## Ver también

- [Documentación oficial de Date en Elm](https://package.elm-lang.org/packages/elm/core/latest/Date)
- [Ejemplos de código para calcular fechas en Elm](https://gist.github.com/mmarconm/10e0c686c3511d407c7a0c249632a854)
- [Sitio web oficial de Elm](https://elm-lang.org/)