---
title:                "Comparando dos fechas"
html_title:           "Gleam: Comparando dos fechas"
simple_title:         "Comparando dos fechas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Por qué
A veces, cuando estamos programando, es necesario comparar dos fechas para realizar ciertas acciones. En Gleam, esta tarea es muy sencilla y puede ahorrarnos tiempo y esfuerzo en nuestro código.

## Cómo hacerlo
Para comparar dos fechas en Gleam, podemos utilizar el módulo `Time` y su función `diff_in_seconds` que nos permite obtener la diferencia en segundos entre dos fechas. Veamos un ejemplo:

```Gleam
import Time

let fecha1 = Time.from_date(2020, 1, 1)
let fecha2 = Time.from_date(2020, 1, 2)

let diferencia = Time.diff_in_seconds(fecha1, fecha2)

Log.info("La diferencia en segundos es {}", [diferencia])

// Resultado:
// La diferencia en segundos es 86400
```

En este ejemplo, creamos dos fechas diferentes utilizando `Time.from_date` y luego utilizamos la función `diff_in_seconds` para obtener la diferencia en segundos entre ellas. Podemos utilizar esta información para realizar cualquier acción que necesitemos en nuestro código.

## Profundizando
La función `diff_in_seconds` en realidad hace uso del módulo `Duration` de Gleam, que nos permite trabajar con intervalos de tiempo y realizar diferentes operaciones con ellos. Podemos obtener la diferencia no solo en segundos, sino también en horas, días, meses, etc.

Además, Gleam también nos ofrece la función `is_before` en el módulo `Time` para comparar si una fecha es anterior a otra, y la función `is_after` para verificar si es posterior. Estas funciones pueden ser muy útiles en diferentes situaciones y nos permiten tener un control preciso de nuestras fechas en el código.

## Ver también
- Documentación oficial de Gleam sobre el módulo `Time`: https://gleam.run/modules/time
- Documentación oficial de Gleam sobre el módulo `Duration`: https://gleam.run/modules/duration