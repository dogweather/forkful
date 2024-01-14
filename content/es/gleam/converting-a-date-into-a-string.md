---
title:                "Gleam: Convirtiendo una fecha en una cadena"
simple_title:         "Convirtiendo una fecha en una cadena"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/es/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Por qué

Convertir una fecha a una cadena de texto es una tarea común en la programación, especialmente cuando se trabaja con datos de fechas en una aplicación. En este artículo, vamos a explorar cómo podemos lograr esto utilizando el lenguaje de programación Gleam.

## Cómo hacerlo

Para convertir una fecha a una cadena de texto en Gleam, podemos utilizar la función `from_date` del módulo `Gleam.Date` y luego llamar a la función `format` con el formato deseado. Por ejemplo, si queremos obtener una cadena en formato `dd/mm/yyyy`, podemos usar el siguiente código:

```Gleam
import Gleam.Date

let date = Date.from_date(2020, 12, 25)

// Convertir a cadena con formato dd/mm/yyyy
let date_string = Date.format(date, "{dd}/{mm}/{yyyy}")

// Imprimir resultado
io.print(date_string) // 25/12/2020
```

Podemos ver que primero creamos una fecha utilizando la función `from_date` y luego llamamos a `format` con el formato deseado. También podemos cambiar el orden de los elementos de la fecha como deseemos en el formato, por ejemplo, `{mm}/{dd}/{yyyy}` produciría `12/25/2020`.

Podemos obtener diferentes formatos de fecha utilizando códigos específicos para cada elemento de la fecha, como `{yyyy}` para el año, `{mm}` para el mes, y `{dd}` para el día. Además, también podemos agregar texto estático o caracteres especiales en el formato, como en el siguiente ejemplo:

```Gleam
// Enero 1, 2020
Date.format(date, "Esta fecha es del día {d} de {m}, {yyyy}")
```

## Profundizando

La función `format` también acepta otros parámetros además del formato, como el idioma y la zona horaria. Podemos especificar el idioma utilizando la función `with_locale` y la zona horaria con `with_time_zone`. Por ejemplo:

```Gleam
import Gleam.Date

let date = Date.from_date(2020, 12, 25)

// Convertir a cadena con formato "dd de mm del yyyy", en español y en la zona horaria "America/Argentina/Buenos_Aires"
let date_string = Date.format(date, "dd de mm del yyyy")
  |> Date.with_locale("es")
  |> Date.with_time_zone("America/Argentina/Buenos_Aires")

// Imprimir resultado
io.print(date_string) // 25 de diciembre del 2020
```

En este ejemplo, especificamos que queremos la fecha en español y en la zona horaria de Buenos Aires. Podemos encontrar la lista de códigos de idioma y zonas horarias disponibles en el [sitio web de Gleam](https://gleam.run/modules/gleam_date/).

## Ver también

- [Documentación de Gleam sobre el módulo Date](https://gleam.run/modules/gleam_date/)
- [Tutorial de Gleam sobre fechas y horas](https://gleam.run/reference/basics/dates_times.html)
- [Códigos de formato de fecha en la especificación de Unicode](https://www.unicode.org/reports/tr35/tr35-52/tr35-dates.html#Date_Format_Patterns)