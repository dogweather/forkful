---
title:                "Analizando una fecha a partir de una cadena de texto"
html_title:           "Bash: Analizando una fecha a partir de una cadena de texto"
simple_title:         "Analizando una fecha a partir de una cadena de texto"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y Por Qué?

El parseo de fechas desde una cadena de texto es el proceso de convertir un string a un tipo de dato fecha. Los programadores lo hacen para manipular y trabajar más fácilmente con fechas.

## Cómo Hacerlo:

En Elm, puedes usar la biblioteca elm/time para parsear fechas. Aquí hay un ejemplo simple de parseo de fechas:

```Elm
import Time

tipoFecha : Time.Zone -> String -> Result String Time.Posix
tipoFecha zona horario = 
  Time.parse zona (Time.fromIsoString horario)

-- Entrada de ejemplo
tipoFecha Time.utc "2021-12-31T23:59:59Z"
-- Salida: Ok (Posix 1640995199)
```

El código anterior intenta convertir un string ISO8601 a Posix time. Las fechas parseadas serán en la zona horaria UTC.

## Inmersión Profunda

Los tipos de fechas pueden ser representados de muchas maneras, pero ISO8601 es un estándar ampliamente aceptado. En Elm, puedes usar la función 'Time.fromIsoString' para parsear cadenas ISO8601. Sin embargo, esta función solo maneja la zona horaria UTC. Si necesitas parsear cadenas de fecha/hora en otras zonas horarias, deberías usar la biblioteca elm/time-extra.

El uso de 'Result' en Elm es una forma segura de manejar posibles errores durante el parseo de fechas. Es una práctica común en Elm donde las funciones que pueden fallar devuelven Result en lugar de arrojar excepciones.

## Ver También

Mira la documentación oficial de Elm para más detalles sobre control de fechas y los paquetes 'Time' y 'Time.Extra': 
- [elm/time](https://package.elm-lang.org/packages/elm/time/latest/)
- [elm/time-extra](https://package.elm-lang.org/packages/justinmimbs/time-extra/latest/)

Por otra parte, si buscas una solución más completa que cubra también formatos de fecha más exóticos, considera la biblioteca [elm/iso8601-date-strings](https://package.elm-lang.org/packages/rtfeldman/elm-iso8601-date-strings/latest/).