---
title:                "Elm: The date actual en la programación de ordenadores"
programming_language: "Elm"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Por qué

¿Alguna vez has necesitado obtener la fecha actual en una aplicación Elm? Si es así, ¡has venido al lugar correcto! En este post, te enseñaremos cómo obtener la fecha actual en Elm de manera sencilla y eficiente.

## Cómo hacerlo

En Elm, podemos utilizar la función `Time.now` del paquete `elm/time` para obtener la fecha y hora actual en Unix timestamps. Luego, podemos convertir el timestamp a una fecha legible utilizando la función `Time.toYearMonthDay` y la función `Time.toHourMinuteSecond`. Veamos un ejemplo de código:

```elm
import Time exposing (..)

fechaActual : Task x (Int, Int, Int)
fechaActual =
    Time.now
        |> Task.map Time.posixToMillis
        |> Task.andThen (\milisegundos -> Time.toYearMonthDay milisegundos |> Task.map .calendar)

horarioActual : Task x (Int, Int, Int)
horarioActual =
    Time.now
        |> Task.map Time.posixToMillis
        |> Task.andThen (\milisegundos -> Time.toHourMinuteSecond milisegundos |> Task.map .clock)

```

En este código, primero importamos el módulo `Time` y luego utilizamos la función `now` para obtener un `Task` que contiene el timestamp actual en milisegundos. Luego, utilizamos la función `andThen` para aplicar dos funciones diferentes a ese `Task`: una para obtener la fecha y otra para obtener la hora. Por último, utilizamos la función `map` para acceder a los valores específicos de la fecha y hora (a través de los campos `calendar` y `clock`, respectivamente).

El resultado de este código sería algo como esto:

```
Fecha actual: (2021, 04, 23)
Hora actual: (16, 30, 27)
```

## Profundizando

Si te interesa saber más sobre cómo obtener la fecha actual en Elm, te recomendamos revisar la documentación del paquete `elm/time` donde encontrarás más información detallada sobre la función `now` y otras funciones relacionadas con el tiempo.

¡Y eso es todo por hoy! Esperamos que este post te haya sido útil para aprender cómo obtener la fecha actual en Elm. ¡Hasta la próxima!

## Ver también

- [Documentación del paquete `elm/time`](https://package.elm-lang.org/packages/elm/time/latest/) 
- [Ejemplos de código en Elm](https://elm-lang.org/examples)
- [Foro de Elm en español](https://discourse.elm-lang.org/c/internacional/espanol)