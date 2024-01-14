---
title:    "Elm: Calculando una fecha en el futuro o pasado"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué
Calcular una fecha en el futuro o en el pasado puede ser útil en muchas ocasiones, como por ejemplo, programar recordatorios o planificar eventos que sucederán en un futuro cercano. En este artículo, exploraremos cómo hacerlo de manera sencilla y efectiva utilizando Elm.

## Cómo hacerlo
Para calcular una fecha en el futuro o en el pasado en Elm, podemos utilizar la función `add` del paquete `Time` y especificar la cantidad de segundos que queremos agregar o restar a una fecha determinada. Por ejemplo, si queremos obtener la fecha dentro de 2 semanas a partir de hoy, podemos utilizar el siguiente código:

```Elm
import Time exposing (add, seconds, now)

fechaFutura = add (2*7*24*60*60 |> seconds) now
```

En este caso, multiplicamos la cantidad de semanas por la cantidad de segundos en una semana y luego agregamos ese resultado a la fecha actual. Esto nos dará como resultado una fecha dos semanas en el futuro. También podemos restar segundos para obtener una fecha en el pasado.

## Profundizando
La función `add` también nos permite especificar valores negativos para restar segundos en lugar de sumarlos. Además, podemos incluir otros valores como días, horas o minutos en lugar de solo segundos. Por ejemplo, si queremos obtener la fecha dentro de 3 días y 5 horas a partir de hoy, podemos utilizar el siguiente código:

```Elm
import Time exposing (add, minutes, hours, days, now)

fechaFutura = add (3|> days + 5|> hours) now
```

Es importante tener en cuenta que la función `add` siempre devuelve como resultado una fecha en formato `Posix`, por lo que puede ser necesario convertirla a un formato más legible mediante la función `fromPosix` del paquete `DateTime`.

## Ver también
- [Documentación de Elm sobre el paquete Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Tutorial sobre Elm en español](https://www.influitive.io/blog/elm-tutorial-en-espanol/)