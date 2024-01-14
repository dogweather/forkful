---
title:                "Elm: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en Elm?

Obtener la fecha actual es una tarea común en la programación, ya sea para mostrarla al usuario o para realizar cálculos basados en la fecha. En Elm, existen diferentes formas y métodos para obtener la fecha actual, así que ¡vamos a explorarlas juntos!

## Cómo obtener la fecha actual en Elm

La forma más sencilla de obtener la fecha actual en Elm es utilizando la función `Time.now` del paquete `elm/time`. Esta función devuelve un `Time` que representa el momento actual en el formato Unix. A continuación, un ejemplo de cómo usarla:

```
import Time exposing (..)

getCurrentDate : Time
getCurrentDate =
    Time.now
```

Podemos utilizar esta función en nuestro código para mostrar la fecha actual al usuario o para realizar cálculos basados en ella.

Otra opción es utilizar las funciones `toTime` y `fromTime` del módulo `Date` del paquete `elm/time`. Estas funciones nos permiten convertir el formato Unix de la fecha actual en un objeto de tipo `Date`, que podemos manipular según nuestras necesidades. A continuación, un ejemplo de cómo usarlas:

```
import Time
import Date exposing (..)

getCurrentDate : Date
getCurrentDate =
    Time.now
        |> Time.toTime
        |> fromTime
```

Este objeto `Date` nos da acceso a diferentes funciones para obtener los componentes de la fecha, como el día, el mes, el año, etc.

## Profundizando en la obtención de la fecha actual en Elm

Para poder utilizar la función `Time.now`, es necesario tener en cuenta que el resultado depende del momento en que se ejecuta la función. Esto significa que si llamamos a la función en diferentes momentos, obtendremos resultados diferentes.

Además, el formato Unix de la fecha actual varía según la zona horaria en la que nos encontremos. Por lo tanto, es importante tener en cuenta esta diferencia al realizar cálculos o comparar fechas.

Otro detalle a tener en cuenta es que las funciones `toTime` y `fromTime` pueden ser costosas en términos de rendimiento, por lo que es importante utilizarlas con cautela y optimizar su uso en nuestro código.

## Ver también

- [Documentación de elm/time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Cheat sheet de Elm](https://elm-lang.org/docs/elm-docs)
- [Tutorial de Elm en español](https://desarrolloweb.com/articulos/tutorial-elm-basico.html)