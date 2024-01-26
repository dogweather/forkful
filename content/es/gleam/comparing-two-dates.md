---
title:                "Comparación de dos fechas"
date:                  2024-01-20T17:33:17.091079-07:00
model:                 gpt-4-1106-preview
simple_title:         "Comparación de dos fechas"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparar dos fechas es medir la diferencia entre ellas. Los programadores lo hacen para manejar eventos, expiraciones, o simplemente para rastrear el tiempo.

## How to:
Gleam no tiene una librería estándar para la manipulación de fechas al momento de mi conocimiento. Sin embargo, podemos usar Erlang mediante funciones incorporadas para conseguir lo que necesitamos. Aquí tienes un ejemplo básico:

```gleam
import gleam/erlang
import gleam/int

fn main() {
  let date1 = erlang.date() // obtiene la fecha actual
  let date2 = {2023, 04, 15} // una fecha específica

  let comparison = erlang.compare(date1, date2) // compara las fechas

  case comparison {
    0 -> "Las fechas son iguales."
    1 -> "La fecha1 es después de la fecha2."
    _ -> "La fecha1 es antes de la fecha2."
  }
}
```

Sample output:
```
"La fecha1 es después de la fecha2."
```

## Deep Dive
La comparación de fechas en Gleam se hace a través de las funciones de Erlang ya que Gleam está diseñado para ser interoperable con Erlang. Históricamente, se prefiere tratar a las fechas como tuplas de tres elementos `{año, mes, día}`. Otras comunidades pueden utilizar objetos de fecha, pero en Erlang y Gleam, las tuplas son suficientes y eficientes. Al comparar, se devuelve 0 si las fechas son iguales, 1 si la primera fecha es más reciente y -1 si es más antigua.

Además de Erlang, puedes explorar librerías de terceros o, si es necesario, crear una función personalizada para tu caso de uso específico. Si se manejan zonas horarias o cálculos más complejos, se deberá considerar la precisión y los límites del tiempo Unix.

## See Also
Visita la documentación de Erlang para más detalles sobre el manejo de fechas y tiempos: [Erlang's Time Functions](http://erlang.org/doc/man/calendar.html).

Para una visión más amplia de cómo Gleam usa las funciones de Erlang, mira [Gleam's Erlang interoperability guide](https://gleam.run/book/tour/erlang-interop.html).

Para entender mejor el sistema de tipos de Gleam, consulta la [Gleam's Type system documentation](https://gleam.run/book/tour/type-system.html).
