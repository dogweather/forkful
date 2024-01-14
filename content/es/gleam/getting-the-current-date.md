---
title:                "Gleam: Obteniendo la fecha actual."
simple_title:         "Obteniendo la fecha actual."
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## ¿Por qué obtener la fecha actual en Gleam?

Obtener la fecha actual es una tarea común en la programación, ya sea para mostrarla en una aplicación o para realizar cálculos basados ​​en el tiempo. En Gleam, esto se puede lograr de manera simple y eficiente utilizando algunas funciones integradas.

## Cómo hacerlo en Gleam

Para obtener la fecha actual en Gleam, podemos usar la función `Time.now()`, que devuelve un registro de fecha y hora. Podemos asignar esto a una variable y luego usar los campos `day`, `month`, `year`, `hour`, `minute` y `second` para obtener información específica sobre la fecha actual.

```Gleam
let fecha_actual = Time.now()
let dia_actual = fecha_actual.day
let mes_actual = fecha_actual.month
let ano_actual = fecha_actual.year

// Output: date: {day: 4, month: 05, year: 2021}
```

También podemos formatear la fecha actual de diferentes maneras utilizando la función `Time.format()` y especificar el formato deseado como argumento. Por ejemplo, si queremos mostrar la fecha actual en el formato `DD/MM/YYYY`, podemos hacerlo de la siguiente manera:

```Gleam
let fecha_actual = Time.now()

let fecha_formateada = Time.format(fecha_actual, "%D/%m/%Y")

// Output: "04/05/2021"
```

## Profundizando en la obtención de la fecha actual

En Gleam, la fecha y la hora se representan como registros, lo que significa que podemos acceder a diferentes partes de ella utilizando los campos mencionados anteriormente. Además, podemos realizar operaciones de cálculo de tiempo utilizando la función `Time.add()` para agregar o restar días, meses, años, horas, minutos o segundos a una fecha específica.

También es importante tener en cuenta que la función `Time.now()` devuelve la fecha y hora en el huso horario actual del sistema, por lo que si necesitamos una fecha y hora en un huso horario diferente, podemos usar la función `Time.translate()` para convertir el registro de fecha y hora al huso horario deseado.

## Ver también

- [Documentación sobre Date and Time en Gleam](https://gleam.run/documentation/std-lib-time)
- [Artículo sobre cómo trabajar con fechas y horas en Gleam](https://medium.com/@gleamlang/guide-to-working-with-dates-and-times-on-gleam-b0071dd36039)
- [Código fuente de Gleam](https://github.com/gleam-lang/gleam)