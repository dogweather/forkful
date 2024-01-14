---
title:    "Gleam: Obteniendo la fecha actual"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en Gleam?

Obtener la fecha actual es una tarea común en muchos programas, ya sea para mostrar la fecha a los usuarios o para realizar cálculos de tiempo. En Gleam, existen diversas formas de obtener la fecha actual y en este artículo te mostraremos cómo hacerlo de manera sencilla y eficiente.

## Cómo hacerlo en Gleam

Para obtener la fecha actual en Gleam, podemos usar la función `Time.now()` que nos devuelve un registro de fecha y hora actual. Veamos un ejemplo:

```Gleam
import Gleam.Time

let current_time = Time.now()
```

En este código, estamos importando el módulo `Time` de Gleam y usando la función `now()` para obtener la fecha y hora actual. La variable `current_time` ahora contiene un registro que contiene los campos `year`, `month`, `day`, `hour`, `minute`, `second` y `millisecond`, lo que nos da una gran cantidad de información para trabajar.

Podemos acceder a estos campos usando la notación de punto, por ejemplo, `current_time.year` nos dará el año actual. Veamos otra forma de obtener la fecha actual usando la función `Time.now_as_list()`:

```Gleam
import Gleam.Time

let current_time = Time.now_as_list()
```

En este caso, la función `now_as_list()` nos devuelve una lista con los mismos campos que `now()`, pero en un orden diferente. También podemos especificar la zona horaria en la que queremos obtener la fecha actual, usando la función `Time.now_in(timezone)`.

## Un poco más profundo en la obtención de la fecha actual

En Gleam, el tipo de dato que representa la fecha y hora es `Time.DateTime`. Podemos usar esta estructura para crear registros de fecha y hora personalizados o realizar operaciones matemáticas con fechas.

Además, la librería estándar de Gleam cuenta con diferentes módulos como `Time.Date`, `Time.Month` y `Time.Day` que nos permiten acceder a información específica de la fecha y realizar operaciones con ella.

## Vea también
- [Documentación oficial de la librería Time en Gleam](https://gleam.run/modules/std/Time.html)
- [Artículo "Getting the Current Date and Time in Gleam" en el blog de Gleam](https://gleam.run/news/getting-the-current-date-and-time-in-gleam.html)