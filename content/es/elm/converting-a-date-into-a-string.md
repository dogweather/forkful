---
title:                "Convirtiendo una fecha en una cadena de texto"
html_title:           "C++: Convirtiendo una fecha en una cadena de texto"
simple_title:         "Convirtiendo una fecha en una cadena de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# Convertir una fecha en una cadena en Elm

## ¿Qué y por qué?

Convertir una fecha en una cadena significa transformar una fecha, que es un valor de tipo `Date` en Elm, en una representación de texto que puede ser manipulada más fácilmente o presentada en un formato más legible. Los programadores hacen esto para la presentación y manipulación de datos.

## Cómo hacer:

En Elm, usamos la biblioteca `elm/time` para tratar con fechas. Aquí hay un ejemplo de cómo convertir una fecha en una cadena:

```elm
import Time

fechaComoCadena : Time.Posix -> String
fechaComoCadena fecha =
    let
        zonaHoraria = Time.here
        hora = Time.toHour zonaHoraria fecha
        minuto = Time.toMinute zonaHoraria fecha
        segundo = Time.toSecond zonaHoraria fecha
    in
    String.fromInt hora ++ ":" ++ String.fromInt minuto ++ ":" ++ String.fromInt segundo
```

Este programa convertirá una fecha en una cadena en la forma de "hora:minuto:segundo".

## Inmersión profunda

### Contexto histórico
Elm tiene una biosfera joven, por lo que se han realizado muchas iteraciones en la forma en que se manejan las fechas. Originalmente, en Elm, la fecha y la hora se proporcionaban a través del módulo `Time` estándar, pero a partir de Elm 0.19, se proporciona a través del paquete `elm/time`. Esta es una mejora significativa ya que proporciona una forma más precisa y clara de trabajar con fechas.

### Alternativas
Además de la biblioteca `elm/time`, también hay bibliotecas de terceros disponibles para manipular las fechas, como `elm-date-extra` y `elm-community/elm-time`. Sin embargo, para la mayoría de los usos, `elm/time` debería ser suficiente.

### Detalles de implementación
La característica más importante a considerar al convertir una fecha en una cadena es la zona horaria. En el ejemplo anterior, usamos `Time.here` para manejar la zona horaria local del usuario. Sin embargo, también puedes especificar la zona horaria explícitamente usando `Time.utc` o `Time.zone`.

## Vea también

- [Documentación de Elm Time](https://package.elm-lang.org/packages/elm/time/latest/)
- [Biblioteca de Elm Date Extra](https://package.elm-lang.org/packages/justinmimbs/date-extra/latest/)
- [Biblioteca de Elm Community Elm Time](https://package.elm-lang.org/packages/elm-community/elm-time/latest/)