---
title:                "Gleam: Calculando una fecha en el futuro o pasado"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

¿Por qué calcular fechas en el pasado o futuro con Gleam?

Calcular fechas puede ser una tarea tediosa y propensa a errores cuando se hace manualmente. Pero con Gleam, puedes usar funciones y métodos específicos para calcular automáticamente fechas en el pasado o futuro. Esto te ahorra tiempo y reduce la posibilidad de errores.

Cómo hacerlo en Gleam

La biblioteca standard de Gleam ofrece varias funciones que te permiten realizar cálculos con fechas. A continuación, se muestra un ejemplo de cómo calcular la fecha de mañana en Gleam:

```Gleam
import gleam/date

let fecha_hoy = date.now()
let fecha_manana = date.add_days(fecha_hoy, 1)

[fecha_hoy, fecha_manana] // salida: [2021-04-23T00:00:00Z, 2021-04-24T00:00:00Z]
```

Puedes ver que usamos la función `date.now()` para obtener la fecha actual y luego la pasamos como primer argumento a la función `date.add_days()`, indicando la cantidad de días que queremos agregar. En este caso, agregamos 1 día a la fecha actual para obtener la fecha de mañana.

Existe una variedad de funciones disponibles en la biblioteca standard de Gleam para calcular fechas en el pasado o futuro, como `add_months`, `add_years`, `subtract_days`, entre otras. Puedes consultar la documentación para obtener más información sobre estas funciones.

Profundizando en el cálculo de fechas

Si deseas un poco más de control sobre tus cálculos de fechas, también puedes usar la biblioteca `gleam/time`, que proporciona funciones más avanzadas como `add` y `subtract` para operaciones con fechas y tiempos.

Además, Gleam también cuenta con paquetes de terceros que pueden ser útiles para usar en cálculos de fechas, como `gleam-calendar` o `gleam-datetime`. Puedes explorar estas opciones y encontrar la mejor solución para tus necesidades específicas.

Ver también

- Documentación de la biblioteca standard de Gleam: https://gleam.run/documentation/standard-library/date
- Gleam-calendar: https://github.com/gleam-lang/gleam-calendar
- Gleam-datetime: https://github.com/PatNowak/gleam-datetime