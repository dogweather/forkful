---
title:                "Obteniendo la fecha actual"
html_title:           "Gleam: Obteniendo la fecha actual"
simple_title:         "Obteniendo la fecha actual"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en Gleam?

Obtener la fecha actual en un programa es importante para realizar tareas como registrar la fecha de creación de un archivo, calcular la diferencia de tiempo entre dos eventos o simplemente mostrar la hora actual en una interfaz de usuario. En Gleam, esta tarea es fácil de lograr gracias a su biblioteca estándar y su función incorporada para obtener la fecha actual.

## Cómo obtener la fecha actual en Gleam

Para obtener la fecha actual en Gleam, simplemente necesitamos importar la biblioteca estándar `time` y llamar a la función `now`. Esto nos dará una estructura de fecha y hora que podemos formatear según nuestras necesidades. Veamos un ejemplo:

```Gleam
import time

let fecha = time.now()

// Formateo personalizado
let fecha_formateada = fecha.format("%d/%m/%Y %H:%M:%S")

// Impresión en consola
io.println(fecha_formateada) // Salida: 21/09/2021 16:30:00
```

¡Y eso es todo! Con solo unas pocas líneas de código, obtenemos la fecha actual y la mostramos en un formato personalizado. Ahora, profundicemos un poco más.

## Profundizando en la obtención de la fecha actual en Gleam

La función `now` de la biblioteca `time` nos devuelve una estructura `Time` de Gleam que contiene información sobre la fecha y la hora actual. Esta estructura tiene varios campos que podemos utilizar para obtener diferentes partes de la fecha, como el día, el mes, el año, la hora, etc.

También podemos utilizar la función `now_utc` para obtener la fecha y la hora en formato UTC en lugar de la hora local. Y si necesitamos una fecha y hora específicas en lugar de la actual, podemos utilizar la función `from_parts` para crear nuestra propia estructura `Time` utilizando los valores proporcionados.

En resumen, obtener la fecha actual en Gleam es una tarea sencilla gracias a su biblioteca estándar y su función `now`. No dude en explorar las opciones disponibles para hacer coincidir con su caso de uso específico.

## Ver también

- Documentación oficial de la biblioteca `time`: https://gleam.run/modules/time
- Más ejemplos de formateo de fecha en Gleam: https://github.com/gleam-lang/gleam/issues/3047
- Ejemplos prácticos de uso de la estructura `Time`: https://dev.to/krthr/practical-time-management-in-gleam-30j3