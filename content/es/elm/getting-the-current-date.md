---
title:    "Elm: Obteniendo la fecha actual"
keywords: ["Elm"]
---

{{< edit_this_page >}}

## Por qué

El obtener la fecha actual es una tarea común en muchos programas. Puede ser útil para registrar eventos, calcular el tiempo transcurrido o simplemente mostrar la fecha a los usuarios.

## Cómo

Para obtener la fecha actual en Elm, podemos utilizar la función `cálcularFecha`, que devuelve un objeto de tipo `Date` con la fecha actual.

```Elm
import Date exposing (..)

fechaActual : Date
fechaActual = calcularFecha
```

Si queremos mostrar la fecha en nuestro programa, podemos usar la función `toString`, que convierte la fecha en una cadena de texto legible para los humanos.

```Elm
import Date exposing (..)

fechaActual : String
fechaActual = Date.toString fechaActual
```

Esto mostrará la fecha en el formato "AAAA-MM-DD". Si queremos personalizar el formato, podemos usar la función `mapToString` y pasarle una función que define cómo queremos que se muestre la fecha.

```Elm
import Date exposing (..)

fechaActual : String
fechaActual = Date.mapToString (\d -> (toString d.yyyy) ++ "/" ++ (toString d.mm) ++ "/" ++ (toString d.dd)) fechaActual
```

Esto mostrará la fecha en el formato "YYYY/MM/DD".

## Profundizando

La función `calcularFecha` utiliza la hora del sistema en el que se está ejecutando el programa para determinar la fecha actual. Esto significa que si cambiamos la hora del sistema, la fecha actual también cambiará.

También podemos obtener la fecha y hora actual con la función `calcularFechaYHora`, que nos devuelve un objeto de tipo `DateTime` que contiene tanto la fecha como la hora.

## Véase también

- Documentación oficial de Elm sobre el módulo Date: https://package.elm-lang.org/packages/elm/time/latest/
- Tutorial de programación en Elm: https://elmprogramming.com/