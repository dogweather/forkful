---
title:                "Convertir una fecha en una cadena de texto."
html_title:           "Elm: Convertir una fecha en una cadena de texto."
simple_title:         "Convertir una fecha en una cadena de texto."
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena es convertir una fecha en un formato legible para humanos, como "27 de marzo de 2021". Los programadores a menudo necesitan convertir fechas en cadenas para fines de visualización o almacenamiento.

## Cómo:

``` Elm
dateToString : Date -> String
dateToString (month, day, year) =
  String.fromInt day ++ " de " ++ getMonthName month ++ " de " ++ String.fromInt year
```

Ejemplo de entrada: (3, 27, 2021)

Salida: "27 de marzo de 2021"

## Profundizando:

Convertir fechas en cadenas ha sido una tarea esencial en la programación desde los inicios. Antes del uso de los sistemas de fecha y hora de la computadora, las fechas se almacenaban en diferentes formatos en diferentes lugares, lo que dificultaba la comprensión y gestión de los datos. 

Existen varias alternativas para convertir fechas en cadenas en Elm, como la función `DateTime.toGregorianCalendar` que convierte una fecha en un objeto de calendario gregoriano, o la biblioteca extra `elm-community/date-extra` que ofrece funciones más avanzadas de conversión de fechas. 

Detrás de escena, la función `dateToString` utiliza la biblioteca `Time` de Elm, que proporciona herramientas útiles para trabajar con fechas y tiempos. Esta función acepta una fecha en formato `Date` y luego utiliza la biblioteca `String` de Elm para concatenar la cadena resultante.

## Ver también:

- La documentación oficial de la función `dateToString` de Elm: https://package.elm-lang.org/packages/elm/time/latest/Time#dateToString
- La biblioteca `elm-community/date-extra`: https://package.elm-lang.org/packages/elm-community/date-extra/latest/
- La biblioteca `Time` de Elm: https://package.elm-lang.org/packages/elm/time/latest/