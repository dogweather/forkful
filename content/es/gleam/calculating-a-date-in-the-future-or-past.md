---
title:                "Gleam: Calculando una fecha en el futuro o pasado"
simple_title:         "Calculando una fecha en el futuro o pasado"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Por qué

Muchas veces, en la programación, necesitamos calcular fechas en el pasado o en el futuro. Esto puede ser necesario para programar eventos, validar datos o simplemente por curiosidad. En Gleam, podemos hacer esto de una manera sencilla y elegante utilizando algunas funciones incorporadas. En esta entrada del blog, aprenderás cómo hacerlo en tan solo unos pocos pasos.

# Cómo hacerlo

Para calcular una fecha en el futuro o en el pasado, necesitamos usar la función `date.shift` de Gleam. Esta función toma tres argumentos: una fecha, una cantidad de tiempo y una unidad de medida de tiempo (días, semanas, meses, etc.). Veamos algunos ejemplos utilizando la fecha de hoy, 23 de mayo de 2021.

```
Gleam
import gleam/time.{Date, Days}

let today = Date.new({ year: 2021, month: 05, day: 23 })
let tomorrow = Date.shift(today, 1, Days) // 2021-05-24
let last_month = Date.shift(today, -1, Months) // 2021-04-23
let next_year = Date.shift(today, 1, Years) // 2022-05-23
```

Como puedes ver, simplemente especificamos la fecha en la que queremos calcular y luego la cantidad de tiempo y la unidad de medida que queremos agregar o restar.

También podemos obtener la fecha actual utilizando la función `Date.now()` y luego aplicar la función `Date.shift` para obtener una fecha en el futuro o en el pasado a partir de la fecha actual.

# Profundizando

Puede que te estés preguntando cómo Gleam maneja las diferentes longitudes de los meses o los años bisiestos. No te preocupes, Gleam se encarga de todo eso por ti. Utiliza el calendario gregoriano utilizado por la mayoría de los países en la actualidad y tiene en cuenta los cambios en la duración de los meses y los años bisiestos.

Otra cosa interesante es que Gleam también te permite personalizar el calendario que se utiliza para el cálculo de fechas. Puedes proporcionar un calendario personalizado usando solo unos pocos pasos y hacer que la función `date.shift` lo utilice.

# Ver también

- Documentación oficial de Gleam sobre la función `date.shift`: https://gleam.run/modules/time.html#date-shift
- Ejemplos prácticos de Gleam en GitHub: https://github.com/gleam-lang/gleam/blob/master/examples/time/day_of_the_week.gleam
- Ejercicio de cálculo de fecha en el futuro utilizando Gleam: https://dayssincelasttweet.com/exercise-deadline/