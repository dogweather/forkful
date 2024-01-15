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

## Por qué

Convertir una fecha a un string puede ser útil para mostrar información de manera más legible en una interfaz de usuario, generar nombres de archivos o formatear fechas para ser incluidas en URL. En general, es una habilidad importante para tener en la programación.

## Cómo hacerlo

Para convertir una fecha a un string en Gleam, se puede utilizar la función `Date.to_string` y especificar el formato deseado. Aquí hay un ejemplo:

```Gleam
let fecha = Date.from_string("2021-06-15", "YYYY-MM-DD")
let fecha_string = Date.to_string(fecha, "DD/MM/YYYY")
println(fecha_string) // Salida: "15/06/2021" 
```

## Inmersión profunda

La función `Date.to_string` acepta cualquier formato compatible con la librería de formateo de fechas de Rust (la cual Gleam utiliza). Además de especificar el año, mes y día con "YYYY", "MM" y "DD", también se pueden incluir información sobre la hora, minutos, segundos y zona horaria. Por ejemplo:

- `"HH:mm:ss"`: escribe la hora en formato de 24 horas (13:45:30)
- `"hh:mm:ss aa"`: escribe la hora en formato de 12 horas con AM o PM (01:45:30 PM)
- `"ZZ"`: escribe la zona horaria en formato de dos dígitos (UTC: +00, Nueva York: -04)
- `"ZZZ"`: escribe la zona horaria en formato de tres dígitos con minutos (UTC: +00:00, Nueva York: -04:00)

Para ver una lista completa de los formatos disponibles, se puede consultar la documentación de la librería de formateo de fechas de Rust.

## Ver también

- [Documentación de la librería de formateo de fechas de Rust](https://doc.rust-lang.org/std/time/format/index.html)
- [Guía de Gleam sobre tipos de datos](https://gleam.run/book/tour/types.html#af29-cada-bbf7-tvot0)
- [Página de inicio de Gleam](https://gleam.run/)