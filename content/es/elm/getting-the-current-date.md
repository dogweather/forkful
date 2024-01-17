---
title:                "Obtener la fecha actual"
html_title:           "Elm: Obtener la fecha actual"
simple_title:         "Obtener la fecha actual"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/elm/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?
Obtener la fecha actual es una funcionalidad muy útil en la programación. Permite a los desarrolladores obtener la fecha y hora actual en su código y utilizarla para diversas tareas, como registrar eventos o realizar cálculos basados en el tiempo. Es una herramienta esencial para mantener la precisión y consistencia en las aplicaciones.

## Cómo hacerlo:
Elm proporciona un módulo incorporado llamado `Time`, que incluye una función llamada `now`, que devuelve la fecha y hora actual como un registro de tiempo. Aquí hay un ejemplo de cómo utilizarlo:

```
Elm.Time.now
-- Output: { time = 1631581280000, posix = 1631581280000, zone = "UTC" }
```

La función `now` devuelve un registro con tres valores: `time`, que es la fecha y hora en milisegundos desde el 1 de enero de 1970, `posix`, que es la misma fecha y hora en formato POSIX y `zone`, que es la zona horaria en la que se ejecuta el código.

Si solo quieres obtener la fecha actual sin la hora, puedes utilizar la función `Date.now` en lugar de `Time.now`. Aquí hay un ejemplo:

```
Elm.Date.now
-- Output: { day = 14, month = 9, year = 2021 }
```

## Profundizando:
La necesidad de obtener la fecha actual en la programación es casi tan antigua como la propia programación. Antes de que los lenguajes de programación modernos tuvieran esta funcionalidad incorporada, los desarrolladores tenían que utilizar funciones más complicadas para obtener la fecha y hora actual.

Hay varios métodos alternativos para obtener la fecha y hora actual en Elm, como utilizar bibliotecas externas o utilizar servidores para obtener la fecha de manera remota. Sin embargo, el módulo `Time` es la forma más sencilla y confiable de hacerlo en Elm.

A nivel de implementación, `Time.now` utiliza la  función `getTimezoneOffset` de JavaScript para obtener la hora y la zona horaria actuales del sistema. Luego, calcula el registro de tiempo en milisegundos y lo convierte a un registro de tiempo de Elm.

## Ver también:
- Documentación oficial de Elm sobre el módulo `Time`: https://package.elm-lang.org/packages/elm/time/latest/
- Ejemplos de uso del módulo `Time`: https://elmprogramming.com/elm-time.html