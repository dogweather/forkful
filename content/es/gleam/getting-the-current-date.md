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

Qué y Por Qué?

Obtener la fecha actual es una tarea común en la programación, ya que permite a los programas mostrar la fecha y hora actual al usuario o utilizarla en cálculos y lógica de programación. Los programadores también pueden utilizarla para rastrear la ejecución del programa y monitorear la eficiencia del mismo.

Cómo:

Para obtener la fecha actual en Gleam, utilizamos la función `DateTime.now()`. Este método nos devuelve un objeto de fecha y hora actual, que luego podemos manipular o imprimir según nuestras necesidades.

```Gleam
let current_date_time = DateTime.now()
```

El objeto `current_date_time` contiene varias funciones como `day`, `month` y `year` para acceder a las partes específicas de la fecha y hora. También podemos formatear la salida utilizando la función `format` en conjunto con la plantilla de formato deseada. A continuación se muestra un ejemplo de cómo imprimir la fecha en formato ISO 8601:

```Gleam
let current_date_time = DateTime.now()
IO.print(format"{iso-8601-date}" current_date_time)
```

La salida sería algo así: `2021-09-05`.

Profundizando:

La obtención de la fecha actual puede ser especialmente útil en aplicaciones web para mostrar la hora en diferentes zonas horarias a los usuarios o en aplicaciones de seguimiento de tiempo para calcular el tiempo transcurrido. También existen otras formas de obtener la fecha actual en Gleam, como utilizar librerías externas o acceder al sistema operativo.

Vea También:

- Documentación oficial de la función `DateTime.now()` en Gleam: https://gleam.run/documentation/core-libraries/datetime/
- Ejemplo de la función `now()` en la librería `gleam/datetime`: https://github.com/lpil/gleam/blob/master/lib/gleam/datetime/example/get_intro.now.gleam