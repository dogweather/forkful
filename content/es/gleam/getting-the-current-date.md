---
title:                "Gleam: Obteniendo la fecha actual"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual?

Obtener la fecha actual es un paso importante en cualquier programa que estemos desarrollando. Esto nos permite realizar tareas como registrar el momento exacto en que se realizó una acción o programar ciertas actividades para una fecha en particular. En Gleam, obtener la fecha actual es muy fácil gracias a su librería de gestión de fechas y tiempo.

## Cómo hacerlo

Para obtener la fecha actual en Gleam, simplemente debemos importar la librería `gleam/datetime` y llamar a la función `now()`. Veamos un ejemplo:

```Gleam
import gleam/datetime

let fecha_actual = datetime.now()

debug.log("La fecha actual es: {}", [fecha_actual])
```

El código anterior nos muestra cómo obtener la fecha actual y luego imprimir un mensaje con la misma fecha en formato de cadena de texto. Si ejecutamos este código, obtendremos el siguiente resultado:

```
La fecha actual es: 2021-07-01T12:30:00Z
```

Como podemos ver, la fecha actual se muestra en formato ISO 8601, lo que facilita su uso en otras funciones. Además, podemos especificar una zona horaria si así lo deseamos.

También podemos hacer uso de la función `now_utc()` para obtener la fecha y hora en coordinación universal (UTC). Esto es útil si necesitamos mantener un registro de la fecha y hora sin importar la zona horaria en la que se encuentre el usuario.

## Profundizando

Además de la función `now()`, la librería `gleam/datetime` nos ofrece otras funciones para manipular fechas y horas, como `format()` para convertir una fecha en una cadena de texto con un formato específico y `parse()` para convertir una cadena de texto en una fecha.

También podemos utilizar los módulos `Date` y `Time` para obtener información más detallada sobre una fecha o una hora específica. Por ejemplo, podemos obtener el día de la semana o el mes de una fecha o la hora exacta de una hora determinada.

## Vea también

- Documentación oficial de `gleam/datetime`: https://gleam.run/modules/gleam/datetime
- Ejemplos de uso de `gleam/datetime`: https://github.com/gleam-lang/standard-library/blob/master/gleam/datetime/examples
- Opciones avanzadas para gestionar fechas y horas en Gleam: https://dev.to/gleam_lang/working-with-dates-and-times-in-gleam-2fo7