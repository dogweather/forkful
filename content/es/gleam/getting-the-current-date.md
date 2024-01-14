---
title:    "Gleam: Obtener la fecha actual"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/es/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Por qué obtener la fecha actual con Gleam

La fecha actual puede ser una información muy importante en la programación, ya sea para mostrarla en una aplicación o para hacer cálculos basados en la fecha. Con Gleam, es muy sencillo obtener la fecha actual y aprovecharla en tus proyectos.

## Cómo hacerlo

Para obtener la fecha actual en Gleam, podemos usar la función `DateTime.now()` del módulo `Gleam.DateTime`. Esto nos devolverá un valor de tipo `DateTime` que contiene la fecha y hora actual.

```Gleam
import Gleam.DateTime

current_date = DateTime.now()

IO.debug(current_date)
```

La salida de este código sería algo así:

`#DateTime<2020-12-28T13:30:00Z>`

Podemos ver que la fecha y hora se encuentran en formato ISO 8601, lo que hace que sea muy fácil manipularla o mostrarla en un formato personalizado.

## Profundizando

Además de obtener la fecha y hora actual, Gleam también nos permite realizar operaciones con fechas, como por ejemplo sumar o restar días, meses o años. Esto se puede lograr con el módulo `Gleam.DateTime.Date` y la función `add_days/2`.

```Gleam
import Gleam.DateTime.Date

today = Date.now()
next_week = Date.add_days(today, 7)

IO.debug(next_week)
```

La salida sería la fecha actual sumando siete días:

`#DateTime<2021-01-04T00:00:00Z>`

Además, podemos usar funciones como `to_unix_seconds/1` para obtener la fecha actual en formato de segundos desde el Epoch (1 de enero de 1970) o `to_rfc3339/1` para obtener la fecha en formato RFC3339.

## Ver también

- [Documentación oficial de Gleam sobre DateTime](https://gleam.run/modules/datetime.html)
- [Ejemplo de uso de fechas en Gleam](https://github.com/gleam-lang/gleam/blob/master/lib/gleam/datetime/date.gleam)