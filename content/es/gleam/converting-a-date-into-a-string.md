---
title:                "Convirtiendo una fecha en una cadena"
html_title:           "Gleam: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## ¿Qué y por qué?

Convertir una fecha en una cadena de texto es una tarea común para los programadores. Usualmente se hace para manipular y mostrar fechas en diferentes formatos, como por ejemplo para mostrar la fecha en un formato legible para los usuarios. Además, algunas funciones o herramientas requieren que las fechas sean representadas como cadenas de texto para poder ser procesadas correctamente.

## Cómo:

Gleam proporciona diferentes maneras de convertir fechas en cadenas de texto. Una forma sencilla es utilizando la función ```strftime```, la cual permite formatear una fecha en un estilo personalizado. Por ejemplo, si queremos formatear la fecha actual en formato "DD/MM/AAAA", podríamos utilizar el siguiente código:

```Gleam
import Time

let date = Time.now()
let formatted_date = Time.strftime("%d/%m/%Y", date)

```

El resultado sería "28/02/2021".

Otra opción es utilizar la función ```format``` que viene incluida en el módulo ```Calendar```. Esta función recibe un objeto de fecha y una cadena de formato, y devuelve la fecha en el formato especificado. Por ejemplo:

```Gleam
import Calendar

let date = Calendar.local_now()
let formatted_date = Calendar.format(date, "%B %d, %Y")

```

El resultado sería "febrero 28, 2021".

## Profundizando:

La necesidad de convertir fechas en cadenas de texto ha existido desde los primeros días de la programación. Anteriormente, muchos lenguajes de programación no tenían formas integradas de trabajar con fechas y horas, por lo que era necesario utilizar funciones de biblioteca externas o escribir código personalizado para manejarlas.

Además de las funciones ```stftime``` y ```format```, existen otras opciones para trabajar con fechas en Gleam, como el módulo ```DateTime``` y la función ```parse``` del módulo ```ISO8601```. También es importante tener en cuenta el sistema de tipos en Gleam, que permite trabajar con fechas de forma segura y sin errores comunes relacionados con formatos incorrectos.

## Ver también:

- Documentación oficial de Gleam sobre fechas y tiempos: https://gleam.run/documentation/stdlib_time
- Ejemplos de uso de fechas y tiempos en Gleam: https://github.com/luminous-io/gleam/blob/master/stdlib/time/test/time_test.gleam
- Introducción al sistema de tipos de Gleam: https://gleam.run/book/static-types